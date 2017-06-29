-module(ierl_util).

-export([get_app_version/1, simplify/1]).

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
