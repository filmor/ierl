-module(ierl).

-export([main/1]).


-spec opt_spec_list() -> list().
opt_spec_list() ->
    [
     {install, $i, "install", string, "Install a kernel with this name"},
     {backend, $b, "backend", string, "The backend to install/run"},
     {conn_file, $f, "conn-file", string, "Connection file provided by "
                                          "Jupyter"},
     {log_level, undefined, "log-level", string, "Log level"},
     {help, $h, "help", boolean, "This help text"}
    ].


-spec main([string()]) -> term().
main(Args) ->
    OptSpecList = opt_spec_list(),

    {ok, {ParsedArgs, LeftOver}} = getopt:parse(OptSpecList, Args),

    Help = Args =:= [] orelse proplists:get_value(help, ParsedArgs),

    case Help of
        true ->
            getopt:usage(OptSpecList, "ierl");
        _ ->
            case proplists:get_value(log_level, ParsedArgs) of
                undefined ->
                    ok;
                LogLevel ->
                    application:ensure_all_started(lager),
                    lager:set_loglevel(lager_console_backend,
                                       list_to_atom(LogLevel))
            end,

            case proplists:get_value(install, ParsedArgs) of
                undefined -> run(ParsedArgs);
                Name -> install(Name, ParsedArgs)
            end
    end.


run(ParsedArgs) ->
    {ok, _Deps} = application:ensure_all_started(ierl),

    JsonFile = proplists:get_value(conn_file, ParsedArgs),
    case JsonFile of
        undefined -> error(must_specify_conn_file);
        _ -> ok
    end,

    lager:info("Starting Erlang kernel with connection file ~s", [JsonFile]),

    Backend = list_to_atom(proplists:get_value(backend, ParsedArgs)),

    process_flag(trap_exit, true),
    {ok, Pid} = jupyter:start_kernel(ierlang, JsonFile, Backend),

    receive
        Msg ->
            io:format("Kernel supervisor is down, stopping. (~p)~n", [Msg]),
            application:stop(jupyter),
            init:stop(0)
    end.


install(Name, ParsedArgs) ->
    application:start(jsx),

    Extra = case proplists:get_value(log_level, ParsedArgs) of
                undefined -> [];
                LogLevel -> [<<"--log-level">>, list_to_binary(LogLevel)]
            end,

    Backend = list_to_atom(proplists:get_value(backend, ParsedArgs)),

    case Backend of
        undefined ->
            error(must_define_backend);
        _ ->
            ok
    end,

    lager:info("Building kernel spec..."),
    Spec = ierl_kernelspec:build(Backend, Extra),
    lager:info("Built kernel spec, storing"),
    ierl_kernelspec:write(Name, Spec),
    lager:info("Installed kernel ~s with backend ~s", [Name, Backend]).
