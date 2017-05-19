-module(jup_kernel_backend_stub).

-export([
         init/1
        ]).

-behaviour(jup_kernel_backend).


init(_Args) ->
    state.
