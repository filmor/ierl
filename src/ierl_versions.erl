-module(ierl_versions).

-export([
         get_otp_version/0,
         get_app_version/1
        ]).


-type version() :: binary() | not_found.


-spec get_app_version(atom()) -> version().
get_app_version(Name) ->
    case lists:keyfind(Name, 1, application:loaded_applications()) of
        {_, _, Version} ->
            jup_util:ensure_binary(Version);
        false ->
            not_found
    end.


-spec get_otp_version() -> version().
get_otp_version() ->
    Res = file:read_file(
            filename:join(
              [code:root_dir(), "releases", erlang:system_info(otp_release),
               "OTP_VERSION"
              ]
             )
           ),

    case Res of
        {ok, Version} ->
            string:trim(jup_util:ensure_binary(Version));
        _ ->
            not_found
    end.
