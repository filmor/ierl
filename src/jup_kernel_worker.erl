-module(jup_kernel_worker).

-behaviour(gen_server).

-export([
         start_link/3,
         start_link/4
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
          executing_pid = undefined :: pid() | undefined
         }).


-dialyzer({nowarn_function, register_msg_to_io/1}).

-spec start_link(jupyter:name(), module(), list()) -> {ok, pid()}.
start_link(Name, Backend, BackendArgs) ->
    IOPid = jup_kernel_io:get_pid(Name),
    Args = [self(), IOPid, Backend, BackendArgs],

    gen_server:start_link(?MODULE, Args, []).


-spec start_link(atom(), atom(), module(), list()) -> pid().
start_link(Name, Node, Backend, BackendArgs) ->
    % TODO: Clean up all modules that are pushed to the other node? Use EXIT
    % signal to check whether we lost connection and purge all modules that were
    % loaded from here and didn't exist before.
    IOPid = jup_kernel_io:get_pid(Name),
    jup_util:copy_to_node(Node, Backend),
    Args = [self(), IOPid, Backend, BackendArgs],

    rpc:call(Node, gen_server, start_link, [?MODULE, Args, []]).


init([Pid, IOPid, Backend, BackendArgs]) ->
    erlang:group_leader(IOPid, self()),
    State = #state{
               pid=Pid,
               backend=Backend,
               backend_state=Backend:init(BackendArgs)
              },

    Pid ! init_complete,

    {ok, State}.


handle_info({call, Ref, Func, Args, Msg}, State) ->
    S1 =
    try
        register_msg_to_io(Msg),
        Args1 = Args ++ [Msg, State#state.backend_state],

        {Res, NewState} =
        case erlang:apply(State#state.backend, Func, Args1) of
            {R, S} -> {R, S};
            {R, _Extra, S} -> {R, S}
        end,

        State#state.pid ! {Ref, Res},
        State#state{backend_state=NewState}
    catch
        Type:Reason ->
            State#state.pid !
            {Ref, exec_error, {Type, Reason, erlang:get_stacktrace()}},
            State
    end,

    {noreply, S1};


handle_info({exec_result, Ref, Result, NewState}, State) ->
    % TODO: Use this
    {noreply, State};


handle_info(_Else, State) ->
    % io:write(_Else),
    % State#state.pid ! {log, _Else},
    {noreply, State}.


handle_cast(Cast, _State) ->
    error({invalid_cast, Cast}).


handle_call(Call, _From, _State) ->
    error({invalid_call, Call}).


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    State.


-spec register_msg_to_io(jup_msg:type()) -> ok.
register_msg_to_io(Msg) ->
    % In separate function as io:setopts spec does not allow arbitrary options
    io:setopts([{jup_msg, Msg}]).
