-module(jupyter).

-include("internal.hrl").

-export([
         start_kernel/4
        ]).


-spec start_kernel(Name :: atom(), Filename :: binary(), Backend :: module(),
                   Args :: map()) ->
    {ok, pid()}.

start_kernel(Name, Filename, Backend, Args) ->
    jup_kernel_sup:start_link(Name, Filename, Backend, Args).
