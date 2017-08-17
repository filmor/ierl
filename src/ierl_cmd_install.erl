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
      {cookie, undefined, "cookie", string, "Cookie"},
      {directory, undefined, "output", string, "Instead of using "
       "jupyter kernelspec to install, just write to the given directory"},
      {replace, undefined, "replace", boolean, "Replace an existing kernel "
       "spec with this name"},
      {user, undefined, "user", boolean, "Install to the per-user kernel "
       "registry"},
      {prefix, undefined, "prefix", string, "Specify a prefix to install to, "
       "e.g. an env. The kernelspec will be installed in "
       "PREFIX/share/jupyter/kernels/"}
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
    Name = case maps:get(name, ParsedArgs, undefined) of
               undefined ->
                   case maps:get(node, ParsedArgs, undefined) of
                       undefined ->
                           BName;
                       NodeName ->
                           NodeName1 = string:to_lower(NodeName),
                           NodeName2 =  re:replace(NodeName1, "[^a-z_0-9]",
                                                   "_"),
                           io_lib:format("~s_~s", [BName, NodeName2])
                   end;
               NameInner ->
                   NameInner
           end,

    DisplayName =
    case jup_util:call_if_exported(Backend, display_name, [BackendArgs1],
                                   undefined) of
        undefined ->
            atom_to_list(BName);
        DisplayNameInner ->
            DisplayNameInner
    end,

    DisplayName1 =
    case DisplayName of
        undefined ->
            Name;
        _ ->
            io_lib:format("~s (~s)", [DisplayName, Name])
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
    case maps:get(directory, ParsedArgs, undefined) of
        undefined ->
            {mochitemp:mkdtemp(), true, true};
        Dir ->
            {jup_util:ensure_binary(Dir), false, false}
    end,

    FullPath = filename:join(Path, Name),

    io:format("Writing kernel spec to ~s...~n", [FullPath]),
    ierl_kernelspec:create(Args2#{ path => FullPath }),

    case Install of
        true ->
            io:format("Installing kernelspec ~s...~n", [Name]),

            JKArgs = [kernelspec, install, FullPath]
            ++
            case maps:get(replace, ParsedArgs, undefined) of
                true ->
                    ["--replace"];
                _ ->
                    []
            end
            ++
            case maps:get(user, ParsedArgs, undefined) of
                true ->
                    ["--user"];
                _ ->
                    []
            end
            ++
            case maps:get(prefix, ParsedArgs, undefined) of
                Prefix when is_list(Prefix) ->
                    ["--prefix", Prefix];
                _ ->
                    []
            end,

            case ierl_exec:exec(jupyter, JKArgs) of
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
