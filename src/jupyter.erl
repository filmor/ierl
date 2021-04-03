-module(jupyter).

-include("internal.hrl").

-export([
    start_kernel/4
]).

-type name() :: atom().

-export_type([name/0]).

-spec start_kernel(
    Name :: name(),
    Filename :: binary(),
    Backend :: module(),
    Args :: map()
) -> {ok, pid()}.

start_kernel(Name, Filename, Backend, Args) ->
    jup_kernel_sup:start_link(Name, Filename, Backend, Args).
