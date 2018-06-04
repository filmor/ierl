-module(jup_kernel_worker).

-include("internal.hrl").

-behaviour(gen_server).

-export([
         start_link/4,
         push/3
        ]).


-export([
         init/1,
         handle_call/3,
         handle_info/2,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).


-record(state, {
          pid :: pid(),
          backend :: module(),
          backend_state :: any(),
          exec_pid = undefined :: pid() | undefined,
          busy = false :: boolean(),
          queues :: map() | undefined,
          current = undefined
         }).

-define(QUEUES, [control, shell]).

-dialyzer({nowarn_function, register_msg_to_io/1}).

-spec push(pid(), control | shell, jup_msg:type()) -> ok.
push(Pid, Queue, Msg) ->
    gen_server:cast(Pid, {process, Queue, Msg}).


-spec start_link(jupyter:name(), atom(), module(), list()) ->
    {ok, pid()}.
start_link(_Name, Node, Backend, BackendArgs) when Node =:= node() ->
    Args = [self(), Backend, BackendArgs],
    gen_server:start_link(?MODULE, Args, []);

start_link(_Name, Node, Backend, BackendArgs) ->
    % TODO: Clean up all modules that are pushed to the other node? Use EXIT
    % signal to check whether we lost connection and purge all modules that were
    % loaded from here and didn't exist before.
    jup_util:copy_to_node(Node, Backend),
    Args = [self(), Backend, BackendArgs],

    rpc:call(Node, gen_server, start_link, [?MODULE, Args, []]).


init([Pid, Backend, BackendArgs]) ->
    process_flag(trap_exit, true),
    State = #state{
               pid=Pid,
               backend=Backend,
               backend_state=Backend:init(BackendArgs)
              },

    State1 = init_queues(State),
    State2 = restart_exec_process(State1),

    {ok, State2}.


handle_info({done, BackendState}, State) ->
    State1 = State#state{backend_state=BackendState, busy=false},
    State2 = trigger_execution(State1),
    {noreply, State2};

handle_info({'EXIT', Pid, _Reason}, State)
  when State#state.exec_pid =:= Pid ->
    ?LOG(debug, "Received EXIT on exec process, reason ~p, restarting", [_Reason]),
    State1 = restart_exec_process(State),
    State2 = trigger_execution(State1),
    {noreply, State2};

handle_info({'EXIT', Pid, Reason}, State)
  when State#state.pid =:= Pid ->
    ?LOG(debug, "Received EXIT from parent, stopping", []),
    {stop, Reason, State};

handle_info(_Else, State) ->
    % io:write(_Else),
    % State#state.pid ! {log, _Else},
    ?LOG(debug, "Received unexpected message: ~p", [_Else]),
    {noreply, State}.


handle_cast({process, Queue, Msg}, State) ->
    MsgType = jup_msg:msg_type(Msg),

    ?LOG(debug, "Received request of type ~s on ~s", [MsgType, Queue]),

    NewState =
    case MsgType of
        <<"interrupt_request">> ->
            ?LOG(debug, "Received interrupt request for ~p",
                 [State#state.exec_pid]
                ),
            exit(State#state.exec_pid, interrupt),

            publish_interruption(State),

            State1 = init_queues(State),
            State1;
        <<"execute_request">> ->
            State1 = enqueue(Queue, Msg, State),
            State1;
        _Other ->
            jup_kernel_protocol:process_message(
              State#state.pid, Queue,
              State#state.backend, State#state.backend_state,
              MsgType, Msg
             ),
            State
    end,

    % Always try to trigger an execution
    FinalState = trigger_execution(NewState),

    {noreply, FinalState}.


handle_call(Call, _From, _State) ->
    error({invalid_call, Call}).


terminate(_Reason, State) ->
    exit(State#state.exec_pid, kill).


code_change(_OldVsn, State, _Extra) ->
    State.


-spec register_msg_to_io(jup_msg:type()) -> ok.
register_msg_to_io(Msg) ->
    % In separate function as io:setopts spec does not allow arbitrary options
    io:setopts([{jup_msg, Msg}]).


restart_exec_process(State) when State#state.exec_pid =/= undefined ->
    exit(State#state.exec_pid, restart),
    restart_exec_process(State#state{exec_pid=undefined});

restart_exec_process(State) ->
    ?LOG(debug, "Restarting exec process", []),
    BackendState = State#state.backend_state,
    Pid = State#state.pid,
    WorkerPid = self(),
    Backend = State#state.backend,

    ExecPid =
    spawn_link(
      fun () -> exec_loop(Pid, WorkerPid, Backend, BackendState) end
     ),

    ?LOG(debug, "Started exec process with PID ~p", [ExecPid]),

    State#state{exec_pid=ExecPid, busy=false}.


-spec exec_loop(pid(), pid(), module(), term()) -> no_return().
exec_loop(Pid, WorkerPid, Backend, BackendState) ->
    BackendState1 =
    receive
        {request, Queue, Msg} ->
            register_msg_to_io(Msg),

            <<"execute_request">> = jup_msg:msg_type(Msg),

            {_Res, NewState} =
            jup_kernel_protocol:process_exec(
              Pid, Queue, Backend, BackendState, Msg
             ),

            NewState
    end,

    WorkerPid ! {done, BackendState1},

    exec_loop(Pid, WorkerPid, Backend, BackendState1).


% iterate through the queues and take elements by priority
get_first([], Queues) ->
    {empty, Queues};

get_first([Name | Names], Queues) ->
    case queue:out(maps:get(Name, Queues)) of
        {empty, _Q} ->
            get_first(Names, Queues);
        {{value, Val}, Q} ->
            {{value, Name, Val}, Queues#{ Name => Q }}
    end.


init_queues(State) ->
    ?LOG(debug, "Reinitialising queues", []),
    State#state{queues=maps:from_list([{Q, queue:new()} || Q <- ?QUEUES])}.


trigger_execution(State) when State#state.busy =:= false ->
    case get_first(?QUEUES, State#state.queues) of
        {{value, Queue, Value}, Queues} ->
            State#state.exec_pid ! {request, Queue, Value},
            State#state{busy=true, current={Queue, Value}, queues=Queues};
        _ ->
            State#state{busy=false}
    end;

trigger_execution(State) ->
    State.


enqueue(Port, Message, State) ->
    Queues = State#state.queues,
    Queue = maps:get(Port, Queues),
    State#state{
      queues=Queues#{ Port => queue:in(Message, Queue) }
     }.


publish_interruption(State) when State#state.current =/= undefined ->
    {Port, Msg} = State#state.current,
    Executor = State#state.pid,

    ResMsg = #{
      ename => <<"interrupted">>,
      evalue => <<"">>,
      traceback => [],
      execution_count => <<"interrupted">>
     },

    jup_kernel_executor:iopub(Executor, error, ResMsg, Msg),
    jup_kernel_executor:reply(Executor, Port, error, ResMsg, Msg),
    jup_kernel_executor:status(Executor, status, Msg);

publish_interruption(_State) ->
    ok.
