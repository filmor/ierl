-module(ierl_cmd_install).

-export([
         exec/4,
         opt_spec/0
        ]).


opt_spec() ->
    {
     "Install a Jupyter kernelspec for this backend",
     [
      {name, $n, "name", string, "Install the kernel under this name "
                                 "(defaults to the backend name)"
      },
      {copy, $c, "copy", boolean, "Copy the escript to make a standalone "
      "kernel"},
      {node, undefined, "node", string, "Remote node to run against"},
      {sname, undefined, "sname", string, "Short name for this node "
       "(defaults to the backend name"},
      {cookie, undefined, "cookie", string, "Cookie"}
     ]
    }.


exec({BName, Backend}, ParsedArgs, BackendArgs, BackendSpec) ->
    application:ensure_all_started(jsx),

    % TODO: Default to BName_Node
    Copy = maps:get(copy, ParsedArgs, false),

    BackendArgs1 = lists:foldl(
                     fun (OptSpec, Res) ->
                             Key = element(1, OptSpec),
                             Name = element(3, OptSpec),

                             case maps:get(Key, BackendArgs, undefined) of
                                 undefined ->
                                     Res;
                                 Value ->
                                     Res#{ Name => Value }
                             end
                     end,
                     #{},
                     BackendSpec
                    ),

    % TODO: Nicer generated name instead of BName
    Name = maps:get(name, ParsedArgs, BName),

    DisplayName = jup_util:call_if_exported(
                    Backend, display_name, [BackendArgs1], undefined
                   ),

    DisplayName1 =
    case DisplayName of
        undefined ->
            Name;
        _ ->
            io:format("~s (~s)", [DisplayName, Name])
    end,

    Args = #{
      display_name => DisplayName1,
      backend => Backend,
      backend_name => BName,
      copy => Copy,
      args => BackendArgs1
     },

    Args1 = lists:foldl(
              fun (PName, A) ->
                      forward_arg(PName, ParsedArgs, A)
              end,
              Args,
              [node, sname]
             ),

    Args2 = case maps:get(cookie, ParsedArgs, undefined) of
                undefined ->
                    Args1;
                Val ->
                    Args1#{
                      env => #{
                        'IERL_COOKIE' => jup_util:ensure_binary(Val)
                       }
                     }
            end,

    {Path, Install, Delete} =
    case true of
        true ->
            {mochitemp:mkdtemp(), true, true};
        _ ->
            {<<"">>, false, false}
    end,

    FullPath = filename:join(Path, Name),

    io:format("Writing kernel spec to ~s...~n", [FullPath]),
    ierl_kernelspec:create(Args2#{ path => FullPath }),

    case Install of
        true ->
            io:format("Installing kernelspec ~s...~n", [Name]),
            case ierl_exec:exec(jupyter, [kernelspec, install, FullPath]) of
                {ok, _} ->
                    io:format("Successfully installed kernelspec~n");
                {error, _, Data} ->
                    io:format("Failed installing:~n~n~s~n", [Data])
            end;
        _ ->
            ok
    end,

    case Delete of
        true ->
            io:format("Cleaning up temporary path...~n"),
            mochitemp:rmtempdir(Path);
        _ ->
            ok
    end.


forward_arg(Name, ParsedArgs, Args) ->
    case maps:get(Name, ParsedArgs, undefined) of
        undefined ->
            Args;
        Val ->
            Args#{
              args => maps:put(Name, Val, maps:get(args, Args, #{})),
              Name => Val
             }
    end.
