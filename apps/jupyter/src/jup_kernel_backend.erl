-module(jup_kernel_backend).

-type state() :: term().
-type stacktrace() :: [binary()] | binary().
-type counter() :: integer().
-type exec_err() ::
    {error, Type :: atom(), Reason :: atom(), StackTrace :: stacktrace()}.

-type match() :: iolist() | binary().
-type complete_res() ::
    [match()]
    | {Shift :: non_neg_integer(), [match()]}
    | {
        Start :: non_neg_integer(),
        End :: non_neg_integer(),
        [match()]
    }.

-callback init(Args :: map()) -> state().
-callback opt_spec() -> {Desc :: iodata(), [getopt:option_spec()]}.

-callback execute(Code :: binary(), jup_msg:type(), state()) ->
    {{ok, jup_display:type()} | exec_err(), counter(), state()}.

-callback exec_counter(state()) -> counter().
-callback kernel_info(jup_msg:type(), state()) -> map().

-callback complete(
    Code :: binary(),
    CursorPos :: integer(),
    jup_msg:type(),
    state()
) -> complete_res().

-callback is_complete(Code :: binary(), jup_msg:type(), state()) ->
    complete | invalid | {incomplete, binary()} | incomplete | unknown.

-callback inspect(
    Code :: binary(),
    CursorPos :: integer(),
    DetailLevel :: integer(),
    jup_msg:type(),
    state()
) -> {ok, jup_display:type()} | not_found.

-optional_callbacks(
    [
        opt_spec/0
    ]
).
