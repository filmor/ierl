-module(jup_kernel_remote).

-export([
         start_link/3,
         start_link/4,
         start_loop/4,
         copy_to_node/2
        ]).


-record(state, {
          pid,
          backend,
          backend_state
         }).


-spec start_link(atom(), module(), list()) -> pid().
start_link(Name, Backend, BackendArgs) ->
    IOPid = jup_kernel_io:get_pid(Name),
    spawn_link(?MODULE, start_loop, [self(), IOPid, Backend,
                                           BackendArgs]).

-spec start_link(atom(), atom(), module(), list()) -> pid().
start_link(Name, Node, Backend, BackendArgs) ->
    % TODO: Clean up all modules that are pushed to the other node?
    IOPid = jup_kernel_io:get_pid(Name),
    copy_to_node(Node, Backend),
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
        {call, Ref, Func, Args} ->
            try
                {Res, NewState} =
                erlang:apply(State#state.backend, Func, Args ++
                             [State#state.backend_state]
                            ),
                State#state.pid ! {Ref, Res},
                State#state{backend_state=NewState}
            catch
                Type:Reason ->
                    State#state.pid ! {Ref, exec_error, {Type, Reason}}
            end
    end,
    loop(State1).


-spec copy_to_node(atom(), module()) -> ok | {error, [{module(), term()}]}.
copy_to_node(Node, Backend) ->
    Deps = jup_util:call_if_exported(Backend, deps, [], []),

    case Node =:= node() of
        true ->
            code:ensure_modules_loaded([Backend] ++ Deps);
        _ ->
            do_copy_to_node(Node, [?MODULE, Backend] ++ Deps)
    end.


do_copy_to_node(Node, Modules) ->
    Errs =
    lists:foldl(
      fun (Module, Errors) ->
              % Never override remote modules
              case rpc:call(Node, code, is_loaded, [Module]) of
                  {file, _} ->
                      ok;
                  _ ->
                      case code:get_object_code(Module) of
                          error ->
                              [{Module, no_object_code} | Errors];
                          {_, Bin, Fn} ->
                              case rpc:call(
                                     Node, code, load_binary,
                                     [Module, Fn, Bin]
                                    ) of
                                  {module, _} ->
                                      Errors;
                                  {error, Error} ->
                                      [{Module, Error} | Errors]
                              end
                      end
              end
      end,
      [],
      Modules
     ),

    case Errs of
        [] ->
            ok;
        _ ->
            {error, Errs}
    end.
