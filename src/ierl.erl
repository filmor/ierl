-module(ierl).

-export([main/1]).

main([JsonFile]) ->
    % TODO: Add proper command-line handling.

    {ok, _Deps} = application:ensure_all_started(ierl),
    lager:set_loglevel(lager_console_backend, debug),
    lager:info("Starting Erlang kernel with connection file ~s", [JsonFile]),

    {ok, Pid} = jupyter:start_kernel(
                  ierlang, JsonFile, jup_kernel_backend_stub
                 ),

    % TODO: Start the supervisor via application, set_env to set the options?
    MonitorRef = monitor(process, Pid),
    receive
        {'DOWN', MonitorRef, process, _, _} ->
            lager:info("Kernel supervisor is down, stopping.")
    end.
