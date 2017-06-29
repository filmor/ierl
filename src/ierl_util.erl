-module(ierl_util).

-export([get_app_version/1]).

get_app_version(Name) ->
    case lists:keyfind(Name, 1, application:loaded_applications()) of
        {_, _, Version} ->
            Version;
        false ->
            not_found
    end.
