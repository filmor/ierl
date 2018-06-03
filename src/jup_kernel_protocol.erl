-module(jup_kernel_protocol).

%% @doc
%% Main jupyter procotol implementation

-include("internal.hrl").

-ifdef(DEBUG).
-define(LOG(Severity, Msg, Args), lager:log(Severity, Msg, Args)).
-else.
-define(LOG(Severity, Msg, Args), ok).
-endif.


-export([process_message/6]).

-type queue() :: control | shell.


% TODO: do_status(Executor, starting) at startup

-spec process_message(pid(), queue(), module(), term(), binary(),
                      jup_msg:type()) -> _.
process_message(Executor, Port, Backend, BackendState, MsgType, Msg) ->
    PRes =
    try
        do_process(Executor, Backend, BackendState, MsgType, Msg)
    catch
        Type:Reason ->
            ?LOG(error, "Error in process_message, stacktrace:~n~s",
                 [lager:pr_stacktrace(erlang:get_stacktrace(), {Type, Reason})]
                ),

            {caught_error, Type, Reason}
    end,

    % TODO: Decide on return values, in particular we must pass the returns
    % through such that we can reuse the BackendState

    case PRes of
        {Status, Result} ->
            reply(Executor, Port, Status, Result, Msg);
        noreply ->
            ok;
        not_implemented ->
            ok;
        Status when is_atom(Status) ->
            reply(Executor, Port, Status, #{}, Msg);
        _Other ->
            ?LOG(error, "Invalid process result: ~p", [_Other])
    end,

    PRes.


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
           protocol_version => <<"5.2">>,
           language_info => LanguageInfo
          },

    {ok, C2};


do_process(Executor, Backend, BackendState, <<"execute_request">>, Msg) ->
    status(Executor, busy, Msg),

    Content = Msg#jup_msg.content,
    Code = case maps:get(<<"code">>, Content) of
               <<"">> -> empty;
               Val -> Val
           end,

    iopub(
      Executor, execute_input,
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
    StoreHistory = case Silent of
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
    {Res, ExecCounter, Metadata} =
    Backend:execute(Code, Silent, StoreHistory, Msg, BackendState),

    Res1 =
    case Res of
        {ok, Value} ->
            iopub(
              Executor, execute_result,
              #{
                  execution_count => ExecCounter,
                  data => jup_display:to_map(Value),
                  metadata => Metadata
              },
              Msg
             ),

            {ok, #{ execution_count => ExecCounter, payload => [],
                    user_expressions => #{} }};

        {error, Type, Reason, Stacktrace} ->
            ResMsg = #{
              ename => jup_util:ensure_binary(Type),
              evalue => jup_util:ensure_binary(Reason),
              traceback => [jup_util:ensure_binary(Row) || Row <- Stacktrace]
             },

            iopub(Executor, error, ResMsg, Msg),

            {error, ResMsg#{ execution_count => ExecCounter }}
    end,

    status(Executor, idle, Msg),

    Res1;


do_process(_Executor, Backend, BackendState, <<"is_complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,
    Code = maps:get(<<"code">>, Content),
    case Backend:is_complete(Code, Msg, BackendState) of
        incomplete ->
            {incomplete, #{ indent => <<"  ">> }};
        {incomplete, Indent} ->
            {incomplete, #{ indent => jup_util:ensure_binary(Indent) }};
        Value ->
            Value
    end;


do_process(_Executor, Backend, BackendState, <<"complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,

    Code = maps:get(<<"code">>, Content),
    CursorPos = maps:get(<<"cursor_pos">>, Content),

    case Backend:complete(Code, CursorPos, Msg, BackendState) of
        L when is_list(L) ->
            {ok, #{
               cursor_start => CursorPos, cursor_end => CursorPos,
               matches => [jup_util:ensure_binary(B) || B <- L],
               metadata => #{}
              }
            };
        not_implemented ->
            not_implemented
    end;


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
    ?LOG(debug, "Not implemented: ~s~n~p", [_MsgType, lager:pr(_Msg, ?MODULE)]),
    {error, #{}}.


-spec reply(pid(), term(), atom(), jup_msg:type() | map(), jup_msg:type()) ->
    ok.
reply(Executor, Port, Status, NewMsg, Msg) ->
    ?LOG(debug, "Replying to ~p with status ~p and content ~p",
         [Port, Status, NewMsg]
        ),

    NewMsg1 = case NewMsg of
                  Map when is_map(Map) ->
                      #jup_msg{content=Map};
                  _ ->
                      NewMsg
              end,

    NewMsg2 = #jup_msg{content=(NewMsg1#jup_msg.content)#{ status => Status }},

    Reply = jup_msg:add_headers(
              NewMsg2, Msg,
              to_reply_type(jup_msg:msg_type(Msg))
             ),

    jup_kernel_executor:reply(Executor, Port, Reply).


status(Executor, Status, Parent) ->
    iopub(Executor, status, #{ execution_state => Status }, Parent).


iopub(Executor, MsgType, Msg, Parent) ->
    ?LOG(debug, "Publishing IO ~p:~n~p", [MsgType, Msg]),
    Msg = jup_msg:add_headers(#jup_msg{content=Msg}, Parent, MsgType),
    jup_kernel_executor:iopub(Executor, Msg).


to_reply_type(MsgType) ->
    binary:replace(MsgType, <<"request">>, <<"reply">>).
