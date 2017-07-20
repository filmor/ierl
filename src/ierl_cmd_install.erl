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

    Args = #{ copy => Copy, args => BackendArgs1 },

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

    Name = maps:get(name, ParsedArgs, BName),

    io:format("Building kernel spec...~n"),
    Spec = ierl_kernelspec:build(BName, Backend, Args2),
    io:format("Built kernel spec, storing~n"),
    ierl_kernelspec:write(Name, Spec),
    io:format("Installed kernel ~s with backend ~s~n", [Name, Backend]).


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
