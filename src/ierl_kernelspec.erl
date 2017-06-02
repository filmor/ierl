-module(ierl_kernelspec).

-export([
         write/2,
         build/2
        ]).


% TODO:
%  - Allow installing the escript into the kernel directory


-spec write(file:filename_all(), map()) -> map().
write(Name, FileMap) ->
    Root = filename:join([get_user_path(), Name]),

    lager:info("Writing kernelspec for ~s to ~s...", [Name, Root]),

    maps:map(
      fun (Fn, {copy, Other}) ->
              Path = filename:join([Root, Fn]),
              ok = filelib:ensure_dir(Path),
              lager:info("Copying file ~s to ~s...", [Fn, Path]),
              file:copy(Other, Path);
          (Fn, Data) ->
              Path = filename:join([Root, Fn]),
              ok = filelib:ensure_dir(Path),
              lager:info("Writing file ~s to ~s...", [Fn, Path]),

              ok = file:write_file(Path, Data)
      end,
      FileMap
     ).


-spec build(module(), map()) -> map().
build(Backend, Options) ->
    build(filename:absname(escript:script_name()), Backend, Options).


-spec build(file:filename_all(), module(), map()) -> map().
build(ScriptPath, Backend, Options) ->
    Root = filename:join([get_user_path(), Backend]),

    Args = case maps:find(args, Options) of
               % TODO Process Args
               {ok, Value} -> Value;
               _ -> []
           end,

    Env = case maps:find(env, Options) of
              {ok, Value1} -> Value1;
              _ -> #{}
          end,

    {Files, ScriptPath1} =
    case maps:get(copy, Options, false) of
        true ->
            NewScriptPath = filename:join([Root, "kernel.escript"]),
            { #{ NewScriptPath => {copy, ScriptPath} }, NewScriptPath};
        _Val ->
            {#{}, ScriptPath}
    end,

    Argv = [
            path_to_binary(get_escript_bin()),
            path_to_binary(ScriptPath1),
            <<"kernel">>,
            Backend,
            <<"-f">>, <<"{connection_file}">>
           ] ++ Args,

    Spec = #{
      argv => Argv,
      % TODO Generate display_name from the module by passing the args, also
      % append the node name in case this is a remote connection
      display_name => jup_util:call_if_exported(
                        Backend, display_name, [], Backend
                       ),
      % TODO Get from module
      language => jup_util:call_if_exported(Backend, language, [], erlang),
      env => Env
    },

    Files#{
      <<"kernel.json">> => jsx:encode(Spec, [{space, 1}, {indent, 2}])
    }.


-spec get_os() -> windows | darwin | linux.
get_os() ->
    case os:type() of
        {win32, _} ->
            windows;
        {_, darwin} ->
            darwin;
        _ ->
            linux
    end.


-spec get_user_path() -> file:filename_all().
get_user_path() ->
    L = case get_os() of
            windows ->
                [getenv("APPDATA"), "jupyter", "kernels"];
            darwin ->
                [getenv("HOME"), "Library", "Jupyter", "kernels"];
            linux ->
                [getenv("HOME"), ".local", "share", "jupyter", "kernels"]
        end,

    filename:join(L).


-spec getenv(string()) -> string().
getenv(Name) when is_list(Name) ->
    case os:getenv(Name) of
        false ->
            {error, {not_set, Name}};
        [] ->
            {error, {empty, Name}};
        Value ->
            Value
    end.


-spec get_escript_bin() -> file:filename_all().
get_escript_bin() ->
    {ok, [[Root]]} = init:get_argument(root),
    filename:join([Root, "bin", "escript"]).


-spec path_to_binary(file:filename_all()) -> binary().
path_to_binary(Fn) when is_binary(Fn) ->
    filename:flatten(Fn);
path_to_binary(Fn) ->
    list_to_binary(filename:flatten(Fn)).
