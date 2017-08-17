-module(ierl_util).

-export([get_app_version/1, simplify/1, format_args/1]).

-spec get_app_version(atom()) -> string().
get_app_version(Name) ->
    case lists:keyfind(Name, 1, application:loaded_applications()) of
        {_, _, Version} ->
            Version;
        false ->
            not_found
    end.


-spec simplify(Path :: file:filename_all()) -> file:filename_all().
simplify(Path) ->
    filename:flatten(filename:join(do_simplify(filename:split(Path), []))).

do_simplify([H | T], Res) ->
    Res1 = case H of
               ".." ->
                   case Res of
                       [_ | RT] ->
                           RT;
                       _ ->
                           []
                   end;
               "." ->
                   Res;
               Else ->
                   [Else | Res]
           end,

    do_simplify(T, Res1);

do_simplify([], Res) ->
    lists:reverse(Res).


-spec format_args(map()) -> [binary()].
format_args(Map) ->
    % TODO: Maybe implement some kind of escaping here.
    lists:reverse(
      maps:fold(
        fun (_Key, Value, Res) when Value =:= false; Value =:= undefined ->
                Res;
            (Key, true, Res) ->
                [jup_util:ensure_binary(io_lib:format("--~s", [Key])) | Res];
            (Key, Value, Res) ->
                [jup_util:ensure_binary(io_lib:format("--~s=~s", [Key, Value]))
                 | Res]
        end,
        [],
        Map
       )
     ).

