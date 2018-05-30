-module(jup_kernel_backend).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/4,
        exec_counter/1,

        execute/5,
        kernel_info/2,
        is_complete/3,
        complete/4,
        inspect/5,
        interrupt/2
       ]).


-export([
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).


-callback init(Args :: map()) -> State :: term().


-type callback_res(Res) :: {Res, State :: term()}
                         | {Res, Extra :: map(), State :: term()}.


-type kernel_info_res() :: map().
-callback do_kernel_info(Msg :: jup_msg:type(), State :: term())
    -> callback_res(kernel_info_res()).


-type exec_err() :: {error, Type :: atom(), Reason :: atom(),
                     StackTrace :: [binary()] | binary()}.
-type execute_res() :: {ok, jup_display:type()} | exec_err().
-callback do_execute(Code::binary(), Msg::jup_msg:type(), State::term())
    -> callback_res(execute_res()).


-type complete_res() :: [binary()].
-callback do_complete(Code::binary(), CursorPos::integer(), Msg::jup_msg:type(),
                      State::term())
    -> callback_res(complete_res()).


-type is_complete_res() :: complete | invalid | {incomplete, binary()} |
                           incomplete | unknown.
-callback do_is_complete(Code::binary(), Msg::jup_msg:type(), State::term())
    -> callback_res(is_complete_res()).


-type inspect_res() :: {ok, jup_display:type()} | not_found.
-callback do_inspect(Code::binary(), CursorPos::integer(),
                     DetailLevel::integer(), Msg::jup_msg:type(), State::term())
    -> callback_res(inspect_res()).


-callback do_interrupt(Msg :: jup_msg:type(), State :: term())
    -> callback_res(ok).


-callback opt_spec() -> {Desc :: iodata(), [getopt:option_spec()]}.


-optional_callbacks([
                     do_complete/4,
                     do_is_complete/3,
                     do_inspect/5,
                     do_interrupt/2,
                     opt_spec/0
                    ]).

% Shutdown should be handled here
% -callback do_shutdown(Restart :: boolean()) -> #{}.

% Implement Silent, History, AllowStdin in here


-record(state, {
          name :: jupyter:name(),
          node :: node(),
          backend :: module(),
          worker_pid :: pid(),

          got_initial_state = false :: boolean(),
          exec_queue :: queue:queue(),
          ctrl_exec_queue :: queue:queue(),
          exec_counter = 0 :: integer(),

          do_kernel_info :: function() | undefined,
          do_execute :: function() | undefined,
          do_complete :: function() | undefined,
          do_is_complete :: function() | undefined,
          do_inspect :: function() | undefined,
          do_interrupt :: function() | undefined
         }).


-define(FWD_CALL(Name, Args, Msg, State),
        case State#state.Name of
            undefined ->
                not_implemented;
            Fun ->
                Fun(Args, Msg)
        end
       ).

-spec start_link(jupyter:name(), node(), module(), map()) -> {ok, pid()}.
start_link(Name, Node, Backend, BackendArgs) ->
    {module, _} = code:ensure_loaded(Backend),
    gen_server:start_link(
      ?JUP_VIA(Name, backend), ?MODULE, [Name, Node, Backend, BackendArgs], []
     ).


-type call_res(T) :: T | not_implemented.

-spec exec_counter(jupyter:name()) -> integer().
exec_counter(Name) ->
    do_call(Name, exec_counter).

-spec execute(jupyter:name(), binary(), boolean(), boolean(), jup_msg:type()) ->
    {execute_res(), ExecCounter :: integer(), Metadata :: map()}.
execute(Name, Code, Silent, StoreHistory, Msg) ->
    do_call(Name, {execute, Code, Silent, StoreHistory, Msg}).

-spec kernel_info(jupyter:name(), jup_msg:type()) -> kernel_info_res().
kernel_info(Name, Msg) ->
    do_call(Name, do_kernel_info, [], Msg).

-spec is_complete(jupyter:name(), binary(), jup_msg:type()) ->
    call_res(is_complete_res()).
is_complete(Name, Code, Msg) ->
    do_call(Name, do_is_complete, [Code], Msg).

-spec complete(jupyter:name(), binary(), integer(), jup_msg:type()) ->
    call_res(complete_res()).
complete(Name, Code, CursorPos, Msg) ->
    do_call(Name, do_complete, [Code, CursorPos], Msg).

-spec inspect(jupyter:name(), binary(), integer(), integer(), jup_msg:type())
    -> call_res(inspect_res()).
inspect(Name, Code, CursorPos, DetailLevel, Msg) ->
    do_call(Name, do_inspect, [Code, CursorPos, DetailLevel], Msg).

-spec interrupt(jupyter:name(), jup_msg:type()) -> call_res(ok).
interrupt(Name, Msg) ->
    do_call(Name, do_interrupt, [], Msg).

init([Name, Node, Backend, BackendArgs]) ->
    {ok, WorkerPid} =
    case Node =:= node() of
        true ->
            jup_kernel_worker:start_link(Name, Backend, BackendArgs);
        _ ->
            jup_kernel_worker:start_link(Name, Node, Backend, BackendArgs)
    end,

    Get =
    fun (FName, Arity) ->
            case erlang:function_exported(Backend, FName, Arity) of
                true ->
                    fun (Args, Msg) when length(Args) =:= Arity - 2 ->
                            Ref = make_ref(),
                            WorkerPid ! {call, Ref, FName, Args, Msg},

                            receive
                                {Ref, Result} ->
                                    Result;
                                {Ref, exec_error, {Type, Reason, St}} ->
                                    lager:error("~p:~p~n~p", [Type, Reason,
                                                              St]),
                                    {error, Type, Reason, St}
                            end
                    end;
                _ ->
                    undefined
            end
    end,

    case erlang:function_exported(Backend, do_kernel_info, 2) of
        false ->
            error({must_define_kernel_info, Backend});
        _ ->
            ok
    end,

    case erlang:function_exported(Backend, do_execute, 3) of
        false ->
            error({must_define_execute, Backend});
        _ ->
            ok
    end,

    {ok, #state{
            name=Name,
            node=Node,
            backend=Backend,
            worker_pid=WorkerPid,

            do_execute=Get(do_execute, 3),
            do_complete=Get(do_complete, 4),
            do_inspect=Get(do_inspect, 5),
            do_is_complete=Get(do_is_complete, 3),
            do_kernel_info=Get(do_kernel_info, 2),
            do_interrupt=Get(do_interrupt, 2)
           }
    }.

handle_info(init_complete, State)
  when State#state.got_initial_state =:= false ->
    % TODO Execute all entries from the exec_queue

    State1 =
    lists:foldr(
      fun ({Action, From}, S) ->
              lager:debug("Handling action ~p", [Action]),
              {reply, Value, NewState} = handle_call(Action, From, S),
              gen_server:reply(From, Value),
              NewState
      end,
      State#state{got_initial_state=true},
      State#state.exec_queue
     ),

    {noreply, State1};

handle_info(_Msg, State) ->
    {noreply, State}.


handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).


handle_call(exec_counter, _From, State) ->
    {reply, State#state.exec_counter, State};


handle_call(Action, From, State)
  when State#state.got_initial_state =:= false ->
    lager:debug("Enqueueing action ~p", [Action]),
    {noreply,
     State#state{exec_queue=[{Action, From} | State#state.exec_queue]}
    };


handle_call({execute, Code, Silent, _StoreHistory, Msg}, _From, State) ->
    ExecCounter = case Silent of
                      true ->
                          State#state.exec_counter;
                      false ->
                          State#state.exec_counter + 1
                  end,

    Res = (State#state.do_execute)([Code], Msg),

    % TODO:
    % - Enqueue commands separately in control and "normal" channel
    % - Always give the control queue precedence
    % - Implement interruption (dropping at least the normal queue)
    % -

    State1 =
    case Res of
        {error, _, _, _} ->
            State;
        _ ->
            State#state{exec_counter=ExecCounter}
    end,

    {reply, {Res, ExecCounter, #{}}, State1};


handle_call({call, Name, Args, Msg}, _From, State) ->
    Res = case Name of
              do_is_complete ->
                  ?FWD_CALL(do_is_complete, Args, Msg, State);
              do_inspect ->
                  ?FWD_CALL(do_inspect, Args, Msg, State);
              do_complete ->
                  ?FWD_CALL(do_complete, Args, Msg, State);
              do_kernel_info ->
                  ?FWD_CALL(do_kernel_info, Args, Msg, State)
          end,

    {reply, Res, State};


handle_call(_Other, _From, _State) ->
    error({invalid_call, _Other}).


code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.

do_call(Name, Call) ->
    gen_server:call(?JUP_VIA(Name, backend), Call, infinity).

do_call(Name, Func, Args, Msg) ->
    gen_server:call(?JUP_VIA(Name, backend), {call, Func, Args, Msg}, infinity).
