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
                                           " Jupyter"},
      {node, undefined, "node", string, "Node to run this kernel on"},
      {sname, undefined, "sname", string, "Short name for this node "
       "(defaults to the backend name"},
      {cookie, undefined, "cookie", string, "Cookie"}
     ]
    }.


exec({BName, Backend}, ParsedArgs, Rest) ->
    {ok, _Deps} = application:ensure_all_started(ierl),

    JsonFile = proplists:get_value(conn_file, ParsedArgs),
    case JsonFile of
        undefined -> error(must_specify_conn_file);
        _ -> ok
    end,

    Node = case proplists:get_value(node, ParsedArgs, undefined) of
               undefined ->
                   node();
               Val ->
                   list_to_atom(Val)
           end,

    SName = proplists:get_value(sname, ParsedArgs, atom_to_list(BName)),

    SName1 = binary_to_atom(
               list_to_binary(
                 [SName, "_", base64:encode(crypto:strong_rand_bytes(6))]
                ),
               utf8
              ),

    net_kernel:start([SName1, shortnames]),
    lager:info("Set node name to ~p", [SName1]),

    case proplists:get_value(cookie, ParsedArgs, undefined) of
        undefined -> ok;
        Val1 -> erlang:setcookie(node(), list_to_atom(Val1))
    end,

    lager:info("Starting Erlang kernel with connection file ~s against ~p",
               [JsonFile, Node]),

    process_flag(trap_exit, true),
    % TODO Parse rest of the args
    Args = #{ node => Node },
    {ok, Pid} = jupyter:start_kernel(ierlang, JsonFile, Backend, Args),

    receive
        Msg ->
            io:format("Kernel supervisor is down, stopping. (~p)~n", [Msg]),
            application:stop(jupyter),
            init:stop(0)
    end.
