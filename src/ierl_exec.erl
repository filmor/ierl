-module(ierl_exec).

-export([exec/2]).


-spec exec( Cmd :: jup_util:string_like(), Args :: [jup_util:string_like()]) ->
    {ok, iolist()}.
exec(Cmd, Args) ->
    Ref = make_ref(),

    Path =
    case os:find_executable(jup_util:ensure_string(Cmd)) of
        false ->
            error({executable_not_found, Cmd});
        P ->
            P
    end,

    Args1 = lists:map(fun jup_util:ensure_string/1, Args),

    {Pid, MonitorRef}
    = erlang:spawn_monitor(
        fun() ->
                R = exec_sync(Path, Args1),
                exit({Ref, R})
        end
       ),

    receive
        {'DOWN', MonitorRef, process, Pid, {Ref, Data}} ->
            Data;
        {'DOWN', MonitorRef, process, Pid, Reason} ->
            exit(Reason)
    end.


exec_sync(Cmd, Args) ->
    P = open_port(
          {spawn_executable, Cmd},
          [binary, use_stdio, stream, exit_status, hide, {args, Args}]
         ),

    exec_receive(P, []).


exec_receive(Port, Acc) ->
    receive
        {Port, {data, Data}} ->
            exec_receive(Port, [Data|Acc]);
        {Port, {exit_status, Status}} ->
            Reversed = lists:reverse(Acc),
            case Status of
                0 ->
                    {ok, Reversed};
                _ ->
                    {error, Status, Reversed}
            end
    end.
