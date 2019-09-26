-module(jup_kernel_iopub_srv).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/2,
        send/2
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
          socket,
          key
         }).

-type state() :: #state{}.


-spec start_link(jupyter:name(), jup_conn_data:type()) -> {ok, pid()}.
start_link(Name, ConnData) ->
    gen_server:start_link(?JUP_VIA(Name, iopub), ?MODULE,
                          [Name, ConnData], []
                         ).


-spec send(jupyter:name(), jup_msg:type()) -> ok.
send(Name, Msg = #jup_msg{}) ->
    gen_server:call(?JUP_VIA(Name, iopub), Msg).


init([Name, ConnData]) ->
    Identity = string:join([atom_to_list(Name), "iopub"], "-"),
    {ok, Socket} = chumak:socket(pub, Identity),
    {ok, _Bind} = chumak:bind(
                    Socket,
                    ConnData#jup_conn_data.transport,
                    binary_to_list(ConnData#jup_conn_data.ip),
                    ConnData#jup_conn_data.iopub_port
                   ),

    StartingStatus =
    jup_msg:add_headers(
      #jup_msg{
         content = #{ execution_state => starting }
        },
      undefined,
      status
     ),

    State = #state{ socket=Socket, key=ConnData#jup_conn_data.signature_key },

    do_send(StartingStatus, State),

    {ok, State}.


-spec handle_info(_, state()) -> {noreply, state()}.
handle_info(_Msg, State) ->
    ?LOG_DEBUG("Unrecognized message: ~p", [_Msg]),
    {noreply, State}.


-spec handle_call(jup_msg:type(), _, state()) -> {reply, ok, state()}.
handle_call(#jup_msg{} = Msg, _From, State) ->
    do_send(Msg, State),
    {reply, ok, State}.


-spec handle_cast(_, state()) -> no_return().
handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).


code_change(_OldVsn, State, _Extra) ->
    State.


terminate(_Reason, _State) ->
    ok.


-spec do_send(jup_msg:type(), state()) -> ok.
do_send(Msg, State) ->
    Encoded = jup_msg:encode(Msg, State#state.key),
    chumak:send_multipart(State#state.socket, Encoded).
