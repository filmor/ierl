-module(jup_kernel_backend).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/4,
        exec_counter/1,

        execute/5,
        kernel_info/2,
        is_complete/3,
        complete/4
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


-callback do_execute(Code::binary(), Publish::function(), Msg::#jup_msg{},
                     State::term())
    -> {
         {ok, Value :: term()}
       | {error, Type :: atom(), Reason :: atom(), StackTrace :: [binary()]},
        State :: term()
       }.


-callback do_complete(Code::binary(), CursorPos::integer(), Msg::#jup_msg{},
                      State::term())
    -> {[binary()], State :: term()}.


-callback do_is_complete(Code::binary(), Msg::#jup_msg{}, State::term())
    -> {
         complete | invalid | {incomplete, binary()} | incomplete | unknown,
         State :: term()
       }.


-callback do_inspect(Code::binary(), CursorPos::integer(),
                     DetailLevel::integer(), Msg::#jup_msg{}, State::term()) ->
    #{}.


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


% -callback do_history



-record(state, {
          name,
          node,
          backend,
          backend_state,
          pending_replies = #{},
          exec_counter = 0,
          do_execute,
          do_complete,
          do_is_complete,
          do_inspect
         }).


-spec start_link(atom(), atom(), module(), list()) -> {ok, pid()}.
start_link(Name, Node, Backend, BackendArgs) ->
    {module, _} = code:ensure_loaded(Backend),
    gen_server:start_link(
      ?JUP_VIA(Name, backend), ?MODULE, [Name, Node, Backend, BackendArgs], []
     ).


exec_counter(Name) ->
    do_call(Name, exec_counter).

execute(Name, Code, Silent, StoreHistory, Msg) ->
    do_call(Name, {execute, Code, Silent, StoreHistory, Msg}).

kernel_info(Name, Msg) -> do_call(Name, {kernel_info, Msg}).
is_complete(Name, Code, Msg) -> do_call(Name, {is_complete, Code, Msg}).
complete(Name, Code, CursorPos, Msg) -> do_call(Name, {complete, Code,
                                                       CursorPos, Msg}).


init([Name, Node, Backend, BackendArgs]) ->
    State =
    case Node =:= node() of
        true ->
            init_local_state(Name, Backend, BackendArgs);
        _ ->
            init_remote_state(Name, Node, Backend, BackendArgs)
    end,

    {ok, State}.


init_local_state(Name, Backend, BackendArgs) ->
    Get =
    fun(FName, Arity) ->
            case erlang:function_exported(Backend, FName, Arity) of
                true ->
                    fun (Args) when length(Args) =:= Arity ->
                            erlang:apply(Backend, FName, Args)
                    end;
                _ ->
                    undefined
            end
    end,

    erlang:group_leader(jup_kernel_io:get_pid(Name), self()),

    #state{
       name=Name,
       node=node(),
       backend=Backend,
       backend_state=Backend:init(BackendArgs),

       do_execute=Get(do_execute, 4),
       do_complete=Get(do_complete, 4),
       do_inspect=Get(do_inspect, 5),
       do_is_complete=Get(do_is_complete, 3)
      }.


init_remote_state(Name, Node, Backend, BackendArgs) ->
    RemotePid = jup_kernel_remote:start_link(Name, Node, Backend, BackendArgs),

    Get =
    fun (FName, Arity) ->
            case erlang:function_exported(Backend, FName, Arity) of
                true ->
                    fun (Args) when length(Args) =:= Arity ->
                            Ref = make_ref(),
                            lager:info("Calling ~p with ~p at ~p", [FName, Args,
                                                                    RemotePid]),
                            RemotePid ! {call, Ref, FName,
                                         lists:droplast(Args)},
                            {
                             receive
                                 {Ref, Result} ->
                                     Result;
                                 {Ref, exec_error, {Type, Reason}} ->
                                     {error, Type, Reason, [<<"internal">>]}
                             end,
                             RemotePid
                            }
                    end;
                _ ->
                    undefined
            end
    end,

    #state{
       name=Name,
       node=Node,
       backend=Backend,
       backend_state=RemotePid,

       do_execute=Get(do_execute, 4),
       do_complete=Get(do_complete, 4),
       do_inspect=Get(do_inspect, 5),
       do_is_complete=Get(do_is_complete, 3)
      }.


handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).

handle_call(exec_counter, _From, State) ->
    {reply, State#state.exec_counter, State};

handle_call({execute, Code, Silent, _StoreHistory, Msg}, _From, State) ->
    io:setopts([{jup_msg, Msg}]),

    ExecCounter = case Silent of
                      true ->
                          State#state.exec_counter;
                      false ->
                          State#state.exec_counter + 1
                  end,

    {Res1, State1} = case State#state.do_execute of
                         undefined ->
                             {not_implemented, State};
                         Fun ->
                             {Res, BState1} = Fun([Code, undefined, Msg,
                                                   State#state.backend_state]),

                             {Res, State#state{exec_counter=ExecCounter,
                                               backend_state=BState1}}
                     end,

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

handle_call({kernel_info, _Msg}, _From, State) ->
    % TODO: Pass on to backend
    {reply, {<<"IErlang">>, <<"0.2">>, <<"Erlang kernel">>}, State};

handle_call({is_complete, Code, Msg}, _From, State) ->
    {Res1, State1} = case State#state.do_is_complete of
                         undefined ->
                             {not_implemented, State};
                         Fun ->
                             {Res, BState1} = Fun([Code, Msg,
                                                   State#state.backend_state]),

                             {Res, State#state{backend_state=BState1}}
                     end,

    Res2 = case Res1 of
               incomplete ->
                   {incomplete, #{ indent => <<"  ">> }};
               {incomplete, Indent} ->
                   {incomplete, #{ indent => Indent }};
               not_implemented ->
                   noreply;
               Value ->
                   Value
           end,

    {reply, Res2, State1};

handle_call({complete, Code, CursorPos, Msg}, _From, State) ->
    {Res1, State1} = case State#state.do_complete of
                         undefined ->
                             {not_implemented, State};
                         Fun ->
                             {Res, BState1} = Fun([Code, CursorPos, Msg,
                                                   State#state.backend_state]),

                             {Res, State#state{backend_state=BState1}}
                     end,

    Res2 = case Res1 of
               L when is_list(L) ->
                   {ok, #{
                      cursor_start => CursorPos, cursor_end => CursorPos,
                      matches => L,
                      metadata => #{}
                     }};
               _ ->
                   noreply
           end,

    {reply, Res2, State1};


handle_call(_Other, _From, _State) ->
    error({invalid_call, _Other}).


code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.

do_call(Name, Call) ->
    gen_server:call(?JUP_VIA(Name, backend), Call).

% process_backend_call(Call, ArgMessage,
