-module(jup_kernel_sup).

%% @doc
%% Top-level supervisor of each running kernel.

-behaviour(supervisor).

-include("internal.hrl").

-export([
         start_link/4,
         stop/1,
         init/1
        ]).


-spec start_link(Name :: term(),
                 FileNameOrData :: jup_connection_file:data() | binary(),
                 Backend :: atom(),
                 Args :: map())
    -> {ok, pid()}.

start_link(Name, ConnData = #jup_conn_data{}, Backend, Args) ->
    supervisor:start_link(
      ?JUP_VIA(Name, kernel_sup), ?MODULE, [Name, ConnData, Backend, Args]
     );

start_link(Name, FileName, Backend, Args) ->
    start_link(Name, jup_connection_file:parse(FileName), Backend, Args).


-spec stop(Name :: term()) -> ok | {error, Reason :: term()}.
stop(Name) ->
    Pid = gproc:where(?JUP_NAME(Name, kernel_sup)),
    Ref = monitor(process, Pid),
    exit(Pid, normal),

    receive
        {'DOWN', Ref, process, Pid, Reason} ->
            case Reason of
                normal ->
                    ok;
                _ ->
                    {error, Reason}
            end
    end.


-spec init([term()]) ->
    {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.

init([Name, ConnData, Backend, Args]) ->
    Socket = fun (PortName, Port) ->
                     A = [Name, PortName, Port, ConnData],
                     #{
                        id => PortName,
                        start => {jup_kernel_socket, start_link, A}
                     }
             end,


    Node = maps:get(node, Args, node()),
    BArgs = maps:get(backend_args, Args, #{}),

    {ok,
     {
      #{},
      [
       #{
         id => heartbeat,
         start => {jup_kernel_heartbeat_srv, start_link, [Name, ConnData]}
       },

       Socket(control, ConnData#jup_conn_data.control_port),
       Socket(shell, ConnData#jup_conn_data.shell_port),

       #{
         id => iopub,
         start => {jup_kernel_iopub_srv, start_link, [Name, ConnData]}
       },

       Socket(stdin, ConnData#jup_conn_data.stdin_port),

       #{ id => io, start => {jup_kernel_io, start_link, [Name]}},

       #{
         id => executor,
         start => {jup_kernel_executor, start_link, [Name, Node, Backend, BArgs]}
       }
      ]
     }
    }.
