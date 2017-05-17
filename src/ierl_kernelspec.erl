-module(ierl_kernelspec).

-export([
         write/2,
         build/2
        ]).


-spec write(file:filename_all(), map()) -> ok.
write(Name, FileMap) ->
    Root = filename:join([get_user_path(), Name]),

    lager:info("Writing kernelspec for ~s to ~s...", [Name, Root]),

    maps:map(
      fun (Fn, Data) ->
              Path = filename:join([Root, Fn]),
              ok = filelib:ensure_dir(Path),
              lager:debug("Writing file ~s to ~s...", [Fn, Path]),

              ok = file:write_file(Path, Data)
      end,
      FileMap
     ).


-spec build(module(), list()) -> map().
build(Backend, Args) ->
    build(filename:absname(escript:script_name()), Backend, Args).


-spec build(file:filename_all(), module(), list()) -> map().
build(ScriptPath, Backend, Args) ->
    Spec = #{
      argv => [
               escript, list_to_binary(ScriptPath), <<"-f">>,
               <<"{connection_file}">>, <<"-b">>, Backend
              ] ++ Args,
      display_name => Backend,
      language => <<"erlang">>
    },

    #{
      <<"kernel.json">> => jsx:encode(Spec, [{space, 1}, {indent, 2}])
    }.


-spec get_os() -> {windows, darwin, linux}.
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
    case get_os() of
        windows ->
            filename:join([os:getenv("APPDATA"), "jupyter", "kernels"]);
        darwin ->
            "~/Library/Jupyter/kernels";
        linux ->
            "~/.local/share/jupyter/kernels"
    end.

