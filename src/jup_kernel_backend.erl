-module(jup_kernel_backend).

-behaviour(gen_server).

-include("internal.hrl").

-export([
        start_link/2,
        execute/2,
        kernel_info/1,
        is_complete/2
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

-callback do_complete(
            Code :: binary(), CursorPos :: integer(), State :: term()
           ) -> #{}.

-callback do_is_complete(Code :: binary()) -> #{}.

% Shutdown should be handled here
% -callback do_shutdown(Restart :: boolean()) -> #{}.

% Implement Silent, History, AllowStdin in here
-callback do_execute(Code :: binary(), UserExpressions :: map()) -> #{}.

%-callback do_inspect(
%            Code :: binary(), CursorPos :: integer(), DetailLevel :: integer(),
%            State :: term()) -> Msg.

% -callback do_history



-record(state, {
          name,
          backend,
          backend_state,
          pending_replies = #{},
          exec_counter = 0
         }).


-spec start_link(Name :: atom(), Backend :: module()) -> {ok, pid()}.
start_link(Name, Backend) ->
    gen_server:start_link(
      ?JUP_VIA(Name, backend), ?MODULE, [Name, Backend], []
     ).


execute(Name, Something) -> do_call(Name, {execute, Something}).
kernel_info(Name) -> do_call(Name, kernel_info).
is_complete(Name, Code) -> do_call(Name, {is_complete, Code}).


init([Name, Backend]) ->
    {ok, #state{
            name=Name,
            backend=Backend,
            backend_state=Backend:init([Name]) % TODO: BackendArgs
           }
    }.

handle_info(_Msg, State) ->
    {noreply, State}.

handle_cast(_Msg, _State) ->
    error({invalid_cast, _Msg}).


handle_call({Call, Args}, From, State) ->
    Backend = State#state.backend,
    %case Backend:Call(Args, State#state.backend_state) of
    %    {reply, Reply, BackendState1} ->
    %        {reply, Reply,
    error(not_implemented);

handle_call({execute, Something}, _From, State) ->
    {reply, #{}, State};

handle_call(kernel_info, _From, State) ->
    {reply, {<<"IErlang">>, <<"0.2">>, <<"Erlang kernel">>}, State};

handle_call({is_complete, Code}, _From, State) ->
    error(not_implemented);
    %{reply, Backend:is_complete(Code)

handle_call(_Other, _From, _State) ->
    error({invalid_call, _Other}).


code_change(_OldVsn, State, _Extra) ->
    State.

terminate(_Reason, _State) ->
    ok.

do_call(Name, Call) ->
    gen_server:call(?JUP_VIA(Name, backend), Call).


% process_backend_call(Call, ArgMessage,
