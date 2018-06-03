-module(jup_kernel_executor).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/4,
        push/3,

        iopub/2,
        reply/3,
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


-spec iopub(pid(), jup_msg:type()) -> ok.
iopub(Pid, Msg) ->
    gen_server:cast(Pid, {iopub, Msg}).

-spec reply(pid(), control | shell, jup_msg:type()) -> ok.
reply(Pid, Queue, Msg) ->
    gen_server:cast(Pid, {reply, Queue, Msg}).

-spec stop(pid()) -> ok.
stop(Pid) ->
    gen_server:cast(Pid, stop).


init([Name, Node, Backend, BackendArgs]) ->
    {ok, WorkerPid} =
    jup_kernel_worker:start_link(Name, Node, Backend, BackendArgs),

    {ok, #state{name=Name, worker_pid=WorkerPid}}.


handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast({push, Port, Msg}, State) ->
    jup_kernel_worker:push(State#state.worker_pid, Port, Msg),
    {noreply, State};


handle_cast({iopub, Msg}, State) ->
    jup_kernel_iopub_srv:send(State#state.name, Msg),
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


handle_call(_Call, _From, _State) ->
    error({invalid_call, _Call}).


code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.
