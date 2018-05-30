-module(jup_kernel_backend).


-callback init(Args :: map()) -> State :: term().

-type callback_res(Res) :: {Res, State :: term()}
                         | {Res, Extra :: map(), State :: term()}.


-type kernel_info_res() :: map().
-callback do_kernel_info(Msg :: jup_msg:type(), State :: term())
    -> callback_res(kernel_info_res()).


-type exec_err() :: {error, Type :: atom(), Reason :: atom(),
                     StackTrace :: [binary()] | binary()}.
-type execute_res() :: {ok, jup_display:type()} | exec_err().
-callback do_execute(Code::binary(), Msg::jup_msg:type(), State::term())
    -> callback_res(execute_res()).


-type complete_res() :: [binary()].
-callback do_complete(Code::binary(), CursorPos::integer(), Msg::jup_msg:type(),
                      State::term())
    -> callback_res(complete_res()).


-type is_complete_res() :: complete | invalid | {incomplete, binary()} |
                           incomplete | unknown.
-callback do_is_complete(Code::binary(), Msg::jup_msg:type(), State::term())
    -> callback_res(is_complete_res()).


-type inspect_res() :: {ok, jup_display:type()} | not_found.
-callback do_inspect(Code::binary(), CursorPos::integer(),
                     DetailLevel::integer(), Msg::jup_msg:type(), State::term())
    -> callback_res(inspect_res()).


-callback do_interrupt(Msg :: jup_msg:type(), State :: term())
    -> callback_res(ok).


-callback opt_spec() -> {Desc :: iodata(), [getopt:option_spec()]}.


-optional_callbacks([
                     do_complete/4,
                     do_is_complete/3,
                     do_inspect/5,
                     do_interrupt/2,
                     opt_spec/0
                    ]).
