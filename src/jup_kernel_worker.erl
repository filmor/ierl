-module(jup_kernel_worker).


-export([
         start_link/3,
         start_link/4,
         start_loop/4
        ]).


-record(state, {
          pid,
          backend,
          backend_state
         }).


-spec start_link(jupyter:name(), module(), list()) -> pid().
start_link(Name, Backend, BackendArgs) ->
    IOPid = jup_kernel_io:get_pid(Name),
    spawn_link(?MODULE, start_loop, [self(), IOPid, Backend, BackendArgs]).


-spec start_link(atom(), atom(), module(), list()) -> pid().
start_link(Name, Node, Backend, BackendArgs) ->
    % TODO: Clean up all modules that are pushed to the other node? Use EXIT
    % signal to check whether we lost connection and purge all modules that were
    % loaded from here and didn't exist before.
    IOPid = jup_kernel_io:get_pid(Name),
    jup_util:copy_to_node(Node, Backend),
    spawn_link(Node, ?MODULE, start_loop, [self(), IOPid, Backend, BackendArgs]).


-spec start_loop(pid(), pid(), module(), list()) -> no_return().
start_loop(Pid, IOPid, Backend, BackendArgs) ->
    erlang:group_leader(IOPid, self()),
    State = #state{
               pid=Pid,
               backend=Backend,
               backend_state=Backend:init(BackendArgs)
              },

    Pid ! init_complete,

    loop(State).


-spec loop(#state{}) -> no_return().
loop(State) ->
    State1 =
    receive
        {call, Ref, Func, Args, Msg} ->
            io:setopts([{jup_msg, Msg}]),
            try
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
                    State#state.pid ! {Ref, exec_error, {Type, Reason}},
                    State
            end
    end,
    loop(State1).
