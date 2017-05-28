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
      }
     ]
    }.


exec({BName, Backend}, ParsedArgs, Rest) ->
    application:ensure_all_started(jsx),

    Extra = case proplists:get_value(log_level, ParsedArgs) of
                undefined -> [];
                LogLevel -> [<<"--log-level">>, list_to_binary(LogLevel)]
            end,

    Name = proplists:get_value(name, ParsedArgs, BName),

    io:format("Building kernel spec...~n"),
    Spec = ierl_kernelspec:build(BName, Extra),
    io:format("Built kernel spec, storing~n"),
    ierl_kernelspec:write(Name, Spec),
    io:format("Installed kernel ~s with backend ~s~n", [Name, Backend]).
