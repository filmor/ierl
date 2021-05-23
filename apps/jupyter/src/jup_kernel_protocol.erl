-module(jup_kernel_protocol).

%% @doc
%% Main jupyter procotol implementation

-include("internal.hrl").

-export([process_message/6, process_exec/5]).

-type queue() :: control | shell.

% TODO: do_status(Executor, starting) at startup

-spec process_message(
    pid(),
    queue(),
    module(),
    term(),
    binary(),
    jup_msg:type()
) -> _.
process_message(Executor, Port, Backend, BackendState, MsgType, Msg) ->
    jup_kernel_executor:status(Executor, busy, Msg),

    PRes =
        try
            do_process(Executor, Backend, BackendState, MsgType, Msg)
        catch
            Type:Reason:Stacktrace ->
                ?LOG_ERROR(
                    fun([]) ->
                        io_lib:format(
                            "Error in process_message, stacktrace:~n~s",
                            [jup_util:format_stacktrace({Type, Reason}, Stacktrace)]
                        )
                    end,
                    []
                ),

                {caught_error, Type, Reason}
        end,

    Return1 =
        case PRes of
            {Status, Result} ->
                jup_kernel_executor:reply(Executor, Port, Status, Result, Msg);
            noreply ->
                ok;
            not_implemented ->
                ok;
            Status when is_atom(Status) ->
                jup_kernel_executor:reply(Executor, Port, Status, #{}, Msg);
            _Other ->
                ?LOG_ERROR("Invalid process result: ~p", [_Other])
        end,

    jup_kernel_executor:status(Executor, idle, Msg),

    Return1.

-spec process_exec(pid(), queue(), module(), term(), jup_msg:type()) -> _.
process_exec(Executor, Queue, Backend, BackendState, Msg) ->
    jup_kernel_executor:status(Executor, busy, Msg),

    Content = Msg#jup_msg.content,
    Code =
        case maps:get(<<"code">>, Content) of
            <<"">> -> empty;
            Val -> Val
        end,

    jup_kernel_executor:iopub(
        Executor,
        execute_input,
        #{
            code => Code,
            execution_count => Backend:exec_counter(BackendState)
        },
        Msg
    ),

    Defaults = #{
        <<"silent">> => false,
        <<"store_history">> => true,
        <<"user_expressions">> => #{},
        <<"allow_stdin">> => true,
        <<"stop_on_error">> => false
    },

    Merged = maps:merge(Defaults, Content),

    Silent = maps:get(<<"silent">>, Merged),
    _StoreHistory =
        case Silent of
            true -> false;
            _ -> maps:get(<<"store_history">>, Merged)
        end,

    % Ignored for now
    _UserExpr = maps:get(<<"user_expressions">>, Merged),
    _AllowStdin = maps:get(<<"allow_stdin">>, Merged),
    _StopOnError = maps:get(<<"stop_on_error">>, Merged),

    % TODO Pass on current exec_counter, such that messages can be ignored, i.e.
    % to implement StopOnError (if error occured on ExecCounter = n => ignore
    % all execution attempts of the same execcounter
    % TODO: Pass Silent and StoreHistory on? Probably better handle this in the
    % worker...
    {Res, ExecCounter, BackendState1} =
        Backend:execute(Code, Msg, BackendState),

    Res1 =
        case Res of
            {ok, Value} ->
                jup_kernel_executor:iopub(
                    Executor,
                    execute_result,
                    #{
                        execution_count => ExecCounter,
                        data => jup_display:to_map(Value),
                        % Metadata
                        metadata => #{}
                    },
                    Msg
                ),

                ResMsg = #{
                    execution_count => ExecCounter,
                    payload => [],
                    user_expressions => #{}
                },

                jup_kernel_executor:reply(Executor, Queue, ok, ResMsg, Msg),
                {ok, BackendState1};
            {error, Type, Reason, Stacktrace} ->
                ResMsg = #{
                    ename => jup_util:ensure_binary(Type),
                    evalue => jup_util:ensure_binary(Reason),
                    traceback => [jup_util:ensure_binary(Row) || Row <- Stacktrace],
                    execution_count => ExecCounter
                },

                jup_kernel_executor:iopub(Executor, error, ResMsg, Msg),
                jup_kernel_executor:reply(Executor, Queue, error, ResMsg, Msg),
                {error, BackendState1}
        end,

    jup_kernel_executor:status(Executor, idle, Msg),
    Res1.

do_process(_Executor, Backend, BackendState, <<"kernel_info_request">>, Msg) ->
    Content = Backend:kernel_info(Msg, BackendState),

    DefaultLanguageInfo = #{
        name => erlang,
        version => unknown,
        file_extension => <<".erl">>
    },

    Defaults = #{
        implementation => erlang_jupyter,
        implementation_version => unknown,
        banner => <<"erlang-jupyter-based Kernel">>
    },

    C1 = maps:merge(Defaults, Content),
    LanguageInfo = maps:merge(
        DefaultLanguageInfo,
        maps:get(language_info, C1, #{})
    ),

    C2 = C1#{
        protocol_version => ?JUP_PROTO_VERSION,
        language_info => LanguageInfo
    },

    {ok, C2};
do_process(_Executor, Backend, BackendState, <<"is_complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,
    Code = maps:get(<<"code">>, Content),
    case Backend:is_complete(Code, Msg, BackendState) of
        incomplete ->
            {incomplete, #{indent => <<"  ">>}};
        {incomplete, Indent} ->
            {incomplete, #{indent => jup_util:ensure_binary(Indent)}};
        Value ->
            Value
    end;
do_process(_Executor, Backend, BackendState, <<"complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,

    Code = maps:get(<<"code">>, Content),
    CursorPos = maps:get(<<"cursor_pos">>, Content),

    {CursorStart, CursorEnd, Matches} =
        case Backend:complete(Code, CursorPos, Msg, BackendState) of
            {Start, End, L} ->
                {Start, End, L};
            {Shift, L} ->
                {max(CursorPos - Shift, 0), CursorPos, L};
            L ->
                {CursorPos, CursorPos, L}
        end,

    Matches1 = jup_util:unique(
        [jup_util:ensure_binary(B) || B <- Matches]
    ),

    {ok, #{
        cursor_start => CursorStart,
        cursor_end => CursorEnd,
        matches => Matches1,
        metadata => #{}
    }};
do_process(_Executor, Backend, BackendState, <<"inspect_request">>, Msg) ->
    Content = Msg#jup_msg.content,

    #{
        <<"code">> := Code,
        <<"cursor_pos">> := CursorPos,
        <<"detail_level">> := DetailLevel
    } = Content,

    % TODO: Specify and handle answer
    Backend:inspect(Code, CursorPos, DetailLevel, Msg, BackendState);
do_process(Executor, _Backend, _BackendState, <<"shutdown_request">>, _Msg) ->
    % Ignore restart for now
    jup_kernel_executor:stop(Executor),
    noreply;
do_process(_Executor, _Backend, _BackendState, _MsgType, _Msg) ->
    ?LOG_DEBUG("Not implemented: ~s~n~p", [_MsgType, _Msg]),
    {error, #{}}.
