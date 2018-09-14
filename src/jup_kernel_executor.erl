-module(jup_kernel_executor).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/4,
        push/3,

        status/3,
        iopub/4,
        reply/5,
        stop/1
       ]).


-export([
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).

% Implement Silent, History, AllowStdin in here


-record(state, {
          name :: jupyter:name(),
          node :: node(),
          worker_pid :: pid()
         }).

-define(NAME(Name), ?JUP_VIA(Name, backend)).


-spec start_link(jupyter:name(), node(), module(), map()) -> {ok, pid()}.
start_link(Name, Node, Backend, BackendArgs) ->
    {module, _} = code:ensure_loaded(Backend),
    gen_server:start_link(
      ?NAME(Name), ?MODULE, [Name, Node, Backend, BackendArgs], []
     ).

-spec push(jupyter:name(), control | shell, jup_msg:type()) -> ok.
push(Name, Queue, Msg) ->
    gen_server:cast(?NAME(Name), {push, Queue, Msg}).


-spec status(pid(), atom(), jup_msg:type()) -> ok.
status(Executor, Status, Parent) ->
    iopub(Executor, status, #{ execution_state => Status }, Parent).


-spec iopub(pid(), jup_msg:msg_type(), jup_msg:type() | map(), jup_msg:type()) ->
    ok.
iopub(Executor, MsgType, Msg, Parent) ->
    ?LOG(debug, "Publishing IO ~p:~n~p", [MsgType, Msg]),
    Msg1 = jup_msg:add_headers(#jup_msg{content=Msg}, Parent, MsgType),
    gen_server:call(Executor, {iopub, Msg1}, 30 * 1000).


-spec reply(pid(), term(), atom(), jup_msg:type() | map(), jup_msg:type()) ->
    ok.
reply(Executor, Port, Status, NewMsg, Msg) ->
    ?LOG(debug, "Replying to ~p with status ~p and content ~p",
         [Port, Status, NewMsg]
        ),

    NewMsg1 = case NewMsg of
                  Map when is_map(Map) ->
                      #jup_msg{content=Map};
                  _ ->
                      NewMsg
              end,

    NewMsg2 = #jup_msg{content=(NewMsg1#jup_msg.content)#{ status => Status }},

    Reply = jup_msg:add_headers(
              NewMsg2, Msg,
              to_reply_type(jup_msg:msg_type(Msg))
             ),

    gen_server:cast(Executor, {reply, Port, Reply}).


-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, {stop, no_restart}).


init([Name, Node, Backend, BackendArgs]) ->
    ?LOG(debug, "Started executor at ~p", [self()]),
    {ok, WorkerPid} =
    jup_kernel_worker:start_link(Name, Node, Backend, BackendArgs),

    {ok, #state{name=Name, worker_pid=WorkerPid}}.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({push, Port, Msg}, State) ->
    jup_kernel_worker:push(State#state.worker_pid, Port, Msg),
    {noreply, State};


handle_cast({reply, Port, Msg}, State) ->
    jup_kernel_socket:send(State#state.name, Port, Msg),
    {noreply, State};

handle_cast({stop, _Restart}, State) ->
    spawn(
      fun () ->
          jup_kernel_sup:stop(State#state.name),
          init:stop(0)
      end
     ),
    {noreply, State}.


handle_call({iopub, Msg}, _From, State) ->
    jup_kernel_iopub_srv:send(State#state.name, Msg),
    {reply, ok, State}.


code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.


-spec to_reply_type(binary()) -> binary().
to_reply_type(MsgType) ->
    binary:replace(MsgType, <<"request">>, <<"reply">>).
