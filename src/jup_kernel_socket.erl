-module(jup_kernel_socket).

%% @doc
%% Router socket implementation, used for control, shell, and stdin
%%
%% Opens a ROUTER socket at the given port and passes decoded messages on to the
%% jup_kernel_dispatcher which uses jup_kernel_protocol for further processing.
%% Answers are sent back from the worker process using send/3.

-behaviour(gen_server).

-include("internal.hrl").

-ignore_xref([
    start_link/4
]).

-export([
    start_link/4,
    send/3
]).

-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    socket :: pid(),
    key :: jup_msg:key(),
    ref :: reference(),
    receiver :: pid()
}).

-spec start_link(
    jupyter:name(),
    atom(),
    integer(),
    jup_connection_file:data()
) -> {ok, pid()}.

start_link(Name, PortName, Port, ConnData) ->
    gen_server:start_link(
        ?JUP_VIA(Name, PortName),
        ?MODULE,
        [Name, PortName, Port, ConnData],
        []
    ).

-spec send(jupyter:name(), atom(), jup_msg:type()) -> ok.
send(Name, PortName, Msg = #jup_msg{}) ->
    gen_server:call(?JUP_VIA(Name, PortName), {send, Msg}).

init([Name, PortName, Port, ConnData]) ->
    Identity = string:join([atom_to_list(Name), atom_to_list(PortName)], "-"),
    {ok, Socket} = chumak:socket(router, Identity),
    {ok, _Bind} = chumak:bind(
        Socket,
        ConnData#jup_conn_data.transport,
        binary_to_list(ConnData#jup_conn_data.ip),
        Port
    ),

    Ref = make_ref(),
    % Self = self(),
    Receiver =
        spawn_link(
            fun() ->
                do_receive_multipart(
                    {Name, PortName},
                    Socket,
                    ConnData#jup_conn_data.signature_key
                )
            end
        ),

    {ok, #state{
        socket = Socket,
        ref = Ref,
        receiver = Receiver,
        key = ConnData#jup_conn_data.signature_key
    }}.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).

handle_call({send, Msg = #jup_msg{}}, _From, State) ->
    ?LOG_DEBUG("Sending: ~p", [Msg]),
    Encoded = jup_msg:encode(Msg, State#state.key),
    chumak:send_multipart(State#state.socket, Encoded),
    {reply, ok, State}.

code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.

-spec do_receive_multipart({jupyter:name(), atom()}, pid(), jup_msg:key()) -> no_return().

do_receive_multipart({Name, PortName}, Socket, SignatureKey) ->
    {ok, Mp} = chumak:recv_multipart(Socket),

    Decoded = jup_msg:decode(Mp, SignatureKey),
    MsgType = jup_msg:msg_type(Decoded),

    ?LOG_DEBUG(
        "Received message of type ~p:~n~p",
        [MsgType, Decoded]
    ),

    jup_kernel_executor:push(Name, PortName, Decoded),

    do_receive_multipart({Name, PortName}, Socket, SignatureKey).
