-module(jup_registry).

-include("internal.hrl").

-export([
         register_name/2,
         unregister_name/1,
         whereis_name/1,
         send/2
        ]).

to_atom(InnerName) ->
    Formatted = io_lib:print(InnerName),
    Str = lists:flatten(Formatted),
    list_to_atom(Str).

register_name({Name, SubName}, Pid) ->
    GprocName = ?JUP_NAME(Name, SubName),
    erlang:register(to_atom({jupyter, Name, SubName}), Pid),
    gproc:register_name(GprocName, Pid).

unregister_name({Name, SubName}) ->
    GprocName = ?JUP_NAME(Name, SubName),
    gproc:unregister_name(GprocName).

whereis_name({Name, SubName}) ->
    GprocName = ?JUP_NAME(Name, SubName),
    gproc:whereis_name(GprocName).

send({Name, SubName}, Msg) ->
    GprocName = ?JUP_NAME(Name, SubName),
    gproc:send(GprocName, Msg).

