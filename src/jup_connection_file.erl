-module(jup_connection_file).

-include("internal.hrl").

-export([
    parse/1
]).

-type data() :: #jup_conn_data{}.

-export_type([data/0]).


-spec parse(binary()) -> data().
parse(Fn) ->
    {ok, Raw} = file:read_file(Fn),
    JsonData = jsx:decode(Raw, [return_maps]),
    V = fun (Name) ->
                maps:get(list_to_binary(Name), JsonData)
        end,

    #jup_conn_data{
       transport = binary_to_existing_atom(V("transport"), utf8),
       ip = V("ip"),

       control_port = V("control_port"),
       heartbeat_port = V("hb_port"),
       shell_port = V("shell_port"),
       iopub_port = V("iopub_port"),
       stdin_port = V("stdin_port"),

       signature_key = {
         parse_signature_scheme(V("signature_scheme")),
         V("key")
        }
      }.


parse_signature_scheme(<<"hmac-sha256">>) ->
    sha256;

parse_signature_scheme(Algo) ->
    error({unknown_signature_scheme, Algo}).
