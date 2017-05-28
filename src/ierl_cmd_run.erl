-module(ierl_cmd_run).

-export([
         exec/3,
         opt_spec/0
        ]).


opt_spec() ->
    {
     "Start a kernel for the given connection file",
     [
      {conn_file, $f, "conn-file", string, "Connection file provided by"
                                           " Jupyter"}
     ]
    }.


exec({BName, Backend}, ParsedArgs, Rest) ->
    {ok, _Deps} = application:ensure_all_started(ierl),

    JsonFile = proplists:get_value(conn_file, ParsedArgs),
    case JsonFile of
        undefined -> error(must_specify_conn_file);
        _ -> ok
    end,

    lager:info("Starting Erlang kernel with connection file ~s", [JsonFile]),

    process_flag(trap_exit, true),
    {ok, Pid} = jupyter:start_kernel(ierlang, JsonFile, Backend),

    receive
        Msg ->
            io:format("Kernel supervisor is down, stopping. (~p)~n", [Msg]),
            application:stop(jupyter),
            init:stop(0)
    end.
