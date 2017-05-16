-module(jupyter).

-include("internal.hrl").

-export([
         start_kernel/3
        ]).


-spec start_kernel(Name :: atom(), Filename :: binary(), Backend :: module()) ->
    {ok, pid()}.

start_kernel(Name, Filename, Backend) ->
    jup_kernel_sup:start_link(Name, Filename, Backend).
