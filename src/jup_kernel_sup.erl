-module(jup_kernel_sup).

-behaviour(supervisor).

-include("internal.hrl").

-export([
         start_link/3,
         stop/1,
         init/1
        ]).


-spec start_link(Name :: term(),
                 FileNameOrData :: #jup_conn_data{} | binary(),
                 Backend :: atom())
    -> {ok, pid()}.

start_link(Name, ConnData = #jup_conn_data{}, Backend) ->
    supervisor:start_link(
      ?JUP_VIA(Name, kernel_sup), ?MODULE, [Name, ConnData, Backend]
     );

start_link(Name, FileName, Backend) ->
    start_link(Name, jup_connection_file:parse(FileName), Backend).


-spec stop(Name :: term()) -> ok.
stop(Name) ->
    supervisor:stop(?JUP_VIA(Name, kernel_sup)).


-spec init([term()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([Name, ConnData, Backend]) ->
    Socket = fun (PortName, Kind, Port) ->
                     Args = [Name, PortName, Kind, Port, ConnData],
                     #{
                        id => PortName,
                        start => {jup_kernel_socket, start_link, Args}
                     }
             end,

    {ok,
     {
      #{},
      [
       #{
         id => heartbeat,
         start => {jup_kernel_heartbeat_srv, start_link, [Name, ConnData]}
       },

       Socket(control, router, ConnData#jup_conn_data.control_port),
       Socket(shell, router, ConnData#jup_conn_data.shell_port),

       #{
         id => iopub,
         start => {jup_kernel_iopub_srv, start_link, [Name, ConnData]}
       },

       Socket(stdin, router, ConnData#jup_conn_data.stdin_port),

       #{ id => io, start => {jup_kernel_io, start_link, [Name]}},

       #{
         id => backend,
         start => {jup_kernel_backend, start_link, [Name, Backend]}
       }
      ]
     }
    }.
