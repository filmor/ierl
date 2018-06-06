-module(jup_kernel_backend).

-type state() :: term().
-type stacktrace() :: [binary()] | binary().
-type counter() :: integer().
-type exec_err() ::
    {error, Type :: atom(), Reason :: atom(), StackTrace :: stacktrace()}.


-callback init(Args :: map()) -> state().
-callback opt_spec() -> {Desc :: iodata(), [getopt:option_spec()]}.


-callback execute(Code::binary(), jup_msg:type(), state()) ->
    {{ok, jup_display:type()} | exec_err(), counter(), state()}.

-callback exec_counter(state()) -> counter().
-callback kernel_info(jup_msg:type(), state()) -> map().
-callback complete(Code::binary(), CursorPos::integer(), jup_msg:type(),
                   state())
    -> [binary()].

-callback is_complete(Code::binary(), jup_msg:type(), state()) ->
    complete | invalid | {incomplete, binary()} | incomplete | unknown.

-callback inspect(Code::binary(), CursorPos::integer(),
                     DetailLevel::integer(), jup_msg:type(), state()) ->
    {ok, jup_display:type()} | not_found.


-optional_callbacks(
   [
    complete/4,
    is_complete/3,
    inspect/5,
    opt_spec/0
   ]).
