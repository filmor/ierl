-ifndef(JUPYTER_RECORDS_HRL).
-define(JUPYTER_RECORDS_HRL, 1).


-record(jup_conn_data, {
          transport :: atom(),
          ip :: binary(),

          control_port :: integer(),
          heartbeat_port :: integer(),
          shell_port :: integer(),
          stdin_port :: integer(),
          iopub_port :: integer(),

          signature_key :: {crypto:hash_algorithms(), binary()}
         }).


-endif.
