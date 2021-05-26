-module(ierl_kernelspec).

-export([create/1]).


% TODO: Allow installing additional files like icons or JS helper scripts

-spec create(map()) -> ok.
create(Options) ->
    #{
      display_name := DisplayName,
      backend := BackendModule,
      backend_name := BackendName,
      args := BackendArgs,
      path := Root,
      copy := Copy
    } = Options,

    ok = filelib:ensure_dir(filename:join([Root, "something"])),

    Env = case maps:find(env, Options) of
              {ok, Value1} -> Value1;
              _ -> #{}
          end,

    ScriptPath = filename:absname(escript:script_name()),

    ScriptPath1 =
    case Copy of
        true ->
            file:copy(ScriptPath, filename:join([Root, "ierl.escript"])),
            <<"{resource_dir}/ierl.escript">>;
        _ ->
            ScriptPath
    end,

    Argv = [
            path_to_binary(get_erl_bin()),
            <<"+B">>, <<"-boot">>, <<"start_clean">>,
            <<"-noshell">>,
            <<"-escript">>, <<"main">>,
            <<"-pz">>, <<"ierl/ierl/ebin">>,
            <<"-run">>, <<"escript">>, <<"start">>,
            <<"-extra">>,
            path_to_binary(ScriptPath1),
            <<"kernel">>,
            BackendName,
            <<"-f">>, <<"{connection_file}">>
           ],

    Argv1 = case ierl_util:format_args(BackendArgs) of
                [] -> Argv;
                BackendArgs1 -> Argv ++ BackendArgs1
            end,

    Spec = #{
      argv => Argv1,
      display_name => jup_util:ensure_binary(DisplayName),
      language => jup_util:call_if_exported(
                    BackendModule, language, [], erlang
                   ),
      interrupt_mode => <<"message">>,
      env => Env
    },

    ok = file:write_file(
      filename:join([Root, "kernel.json"]),
      jsx:encode(Spec, [{space, 1}, {indent, 2}])
     ),

    ok.

-spec get_erl_bin() -> file:filename_all().
get_erl_bin() ->
    {ok, [[Root]]} = init:get_argument(root),
    filename:join([Root, "bin", "erl"]).


-spec path_to_binary(file:filename_all()) -> binary().
path_to_binary(Fn) when is_binary(Fn) ->
    filename:flatten(Fn);
path_to_binary(Fn) ->
    list_to_binary(filename:flatten(Fn)).
