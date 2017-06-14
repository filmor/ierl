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
        inspect/4
       ]).


-export([
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).


-callback init(Args :: [term()]) -> State :: term().


-type callback_res(Res) :: {Res, State :: term()}
                         | {Res, Extra :: map(), State :: term()}.

-type exec_err() :: {error, Type :: atom(), Reason :: atom(),
                     StackTrace :: [binary()] | binary()}.


-callback do_execute(Code::binary(), Publish::function(), Msg::#jup_msg{},
                     State::term())
    -> callback_res({ok, jup_display:type()} | exec_err()).


-callback do_complete(Code::binary(), CursorPos::integer(), Msg::#jup_msg{},
                      State::term())
    -> callback_res([binary()]).


-callback do_is_complete(Code::binary(), Msg::#jup_msg{}, State::term())
    -> callback_res(
         complete | invalid | {incomplete, binary()} | incomplete | unknown
        ).


-callback do_inspect(Code::binary(), CursorPos::integer(),
                     DetailLevel::integer(), Msg::#jup_msg{}, State::term())
    -> callback_res({ok, jup_display:type()} | not_found).


-callback opt_spec() -> [getopt:option_spec()].


-optional_callbacks([
                     do_complete/4,
                     do_is_complete/3,
                     do_inspect/5,
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
          exec_queue = [] :: [tuple()],
          exec_counter = 0 :: integer(),

          do_execute :: function() | undefined,
          do_complete :: function() | undefined,
          do_is_complete :: function() | undefined,
          do_inspect :: function() | undefined
         }).


-define(FWD_CALL(Name, Args, State),
        case State#state.Name of
            undefined ->
                not_implemented;
            Fun ->
                Fun(Args)
        end
       ).

-spec start_link(jupyter:name(), node(), module(), [any()]) -> {ok, pid()}.
start_link(Name, Node, Backend, BackendArgs) ->
    {module, _} = code:ensure_loaded(Backend),
    gen_server:start_link(
      ?JUP_VIA(Name, backend), ?MODULE, [Name, Node, Backend, BackendArgs], []
     ).


-spec exec_counter(jupyter:name()) -> integer().
exec_counter(Name) ->
    do_call(Name, exec_counter).

-spec execute(jupyter:name(), binary(), boolean(), boolean(), #jup_msg{}) -> ok.
execute(Name, Code, Silent, StoreHistory, Msg) ->
    do_call(Name, {execute, Code, Silent, StoreHistory, Msg}).

kernel_info(Name, Msg) ->
    do_call(Name, {kernel_info, Msg}).
is_complete(Name, Code, Msg) ->
    do_call(Name, do_is_complete, [Code, Msg]).
complete(Name, Code, CursorPos, Msg) ->
    do_call(Name, do_complete, [Code, CursorPos, Msg]).
inspect(Name, Code, CursorPos, DetailLevel) ->
    do_call(Name, do_inspect, [Code, CursorPos, DetailLevel]).


init([Name, Node, Backend, BackendArgs]) ->
    WorkerPid =
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
                    fun (Args) when length(Args) =:= Arity - 1 ->
                            Ref = make_ref(),
                            WorkerPid ! {call, Ref, FName, Args},

                            receive
                                {Ref, Result} ->
                                    Result;
                                {Ref, exec_error, {Type, Reason}} ->
                                    {error, Type, Reason, [<<"internal">>]}
                            end
                    end;
                _ ->
                    undefined
            end
    end,

    case erlang:function_exported(Backend, do_execute, 4) of
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

            do_execute=Get(do_execute, 4),
            do_complete=Get(do_complete, 4),
            do_inspect=Get(do_inspect, 5),
            do_is_complete=Get(do_is_complete, 3)
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


handle_call({kernel_info, _Msg}, _From, State) ->
    % TODO: Pass on to backend
    {reply, {<<"IErlang">>, <<"0.2">>, <<"Erlang kernel">>}, State};

handle_call(exec_counter, _From, State) ->
    {reply, State#state.exec_counter, State};

handle_call(Action, From, State)
  when State#state.got_initial_state =:= false ->
    lager:debug("Enqueueing action ~p"),
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

    Res1 = (State#state.do_execute)([Code, undefined, Msg]),
    State1 = State#state{exec_counter=ExecCounter},

    Res2 = case Res1 of
               {ok, Value} ->
                   jup_kernel_protocol:do_iopub(
                     State#state.name,
                     execute_result,
                     #{
                       execution_count => ExecCounter,
                       data => #{
                         <<"text/plain">> =>
                         list_to_binary(io_lib:format("~p", [Value]))
                        },
                       metadata => #{}
                      },
                     Msg
                    ),
                   {ok, #{ execution_count => ExecCounter, payload => [],
                           user_expressions => #{} }};
               {error, Type, Reason, Stacktrace} ->
                   jup_kernel_protocol:do_iopub(
                     State#state.name,
                     error,
                     #{
                       execution_count => ExecCounter,
                       ename => Type,
                       evalue => Reason,
                       traceback => Stacktrace
                      },
                     Msg
                    ),
                   {error, #{
                      ename => Type,
                      evalue => Reason,
                      traceback => Stacktrace,
                      execution_count => ExecCounter
                     }
                   }
           end,

    {reply, Res2, State1};

handle_call({call, Name, Args}, _From, State) ->
    Res = case Name of
              do_is_complete ->
                  ?FWD_CALL(do_is_complete, Args, State);
              do_inspect ->
                  ?FWD_CALL(do_inspect, Args, State);
              do_complete ->
                  ?FWD_CALL(do_complete, Args, State)
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

do_call(Name, Func, Args) ->
    gen_server:call(?JUP_VIA(Name, backend), {call, Func, Args}, infinity).
