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
                                 "(defaults to the backend name"
      },
      {copy, $c, "copy", boolean, "Copy the escript to make a standalone "
      "kernel"},
      {node, undefined, "node", string, "Remote node to run against"}
     ]
    }.


exec({BName, Backend}, ParsedArgs, _Rest) ->
    application:ensure_all_started(jsx),

    % TODO: Default to BName_Node

    Name = proplists:get_value(name, ParsedArgs, BName),
    Copy = proplists:get_value(copy, ParsedArgs, false),

    Args = #{ copy => Copy },

    Args1 = case proplists:get_value(node, ParsedArgs, undefined) of
                undefined ->
                    Args;
                Val ->
                    Args#{
                      args => [<<"--node">>, jup_util:ensure_binary(Val)]
                     }
            end,

    % TODO Parse rest and add to spec

    io:format("Building kernel spec...~n"),
    Spec = ierl_kernelspec:build(BName, Args1),
    io:format("Built kernel spec, storing~n"),
    ierl_kernelspec:write(Name, Spec),
    io:format("Installed kernel ~s with backend ~s~n", [Name, Backend]).
