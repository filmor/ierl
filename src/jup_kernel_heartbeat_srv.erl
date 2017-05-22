-module(jup_kernel_heartbeat_srv).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/2,
        last_heartbeat/1
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
          last_heartbeat,
          receiver,
          ref
         }).


start_link(Name, ConnData) ->
    gen_server:start_link(?JUP_VIA(Name, heartbeat), ?MODULE,
                          [Name, ConnData], []
                         ).


last_heartbeat(Name) ->
    gen_server:call(?JUP_VIA(Name, heartbeat), last_heartbeat).


init([Name, ConnData]) ->
    Identity = string:concat(atom_to_list(Name), "-heartbeat"),
    {ok, Socket} = chumak:socket(rep, Identity),
    {ok, _Bind} = chumak:bind(
                    Socket,
                    ConnData#jup_conn_data.transport,
                    binary_to_list(ConnData#jup_conn_data.ip),
                    ConnData#jup_conn_data.heartbeat_port
                   ),

    timer:sleep(1000),

    Ref = make_ref(),
    Self = self(),

    {ok, #state{
            socket=Socket,
            receiver=spawn_link(fun () -> do_receive(Self, Ref, Socket) end),
            ref=Ref
           }
    }.

handle_info({Ref, _Msg, LastHeartbeat}, State)
  when State#state.ref =:= Ref ->
    {noreply, State#state{last_heartbeat=LastHeartbeat}}.


handle_call(last_heartbeat, _From, State) ->
    {reply, State#state.last_heartbeat, State}.


handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).


code_change(_OldVsn, State, _Extra) ->
    State.


terminate(_Reason, _State) ->
    ok.


do_receive(Pid, Ref, Socket) ->
    {ok, Msg} = chumak:recv(Socket),
    % lager:debug("Got heartbeat message: ~p", [Msg]),
    chumak:send(Socket, Msg),
    Pid ! {Ref, Msg, os:timestamp()},
    do_receive(Pid, Ref, Socket).
