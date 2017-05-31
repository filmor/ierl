-module(ierl_cmd_install).

-export([
         exec/3,
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


exec({BName, Backend}, ParsedArgs, _Rest) ->
    application:ensure_all_started(jsx),

    % TODO: Default to BName_Node
    Name = proplists:get_value(name, ParsedArgs, BName),
    Copy = proplists:get_value(copy, ParsedArgs, false),

    Args = #{ copy => Copy, args => [] },

    Args1 = lists:foldl(
              fun (PName, A) ->
                      forward_arg(PName, ParsedArgs, A)
              end,
              Args,
              [node, sname, cookie]
             ),

    % TODO Parse rest and add to spec

    io:format("Building kernel spec...~n"),
    Spec = ierl_kernelspec:build(BName, Args1),
    io:format("Built kernel spec, storing~n"),
    ierl_kernelspec:write(Name, Spec),
    io:format("Installed kernel ~s with backend ~s~n", [Name, Backend]).


forward_arg(Name, ParsedArgs, Args) ->
    case proplists:get_value(Name, ParsedArgs, undefined) of
        undefined ->
            Args;
        Val ->
            Param = jup_util:ensure_binary(io_lib:format("--~p", [Name])),
            Args#{
              args => maps:get(args, Args, [])
                      ++ [Param, jup_util:ensure_binary(Val)]
             }
    end.
