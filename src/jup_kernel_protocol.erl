-module(jup_kernel_protocol).

-include("internal.hrl").

-export([
         process_message/4,
         do_iopub/4
        ]).


% TODO: do_status(Name, starting) at startup

process_message(Name, Port, MsgType, Msg) ->
    do_status(Name, busy, Msg),
    % TODO: Catch and send error?
    case do_process(Name, Port, MsgType, Msg) of
        {Status, Result} ->
            do_reply(Name, Port, Status, Result, Msg);
        Status when is_atom(Status) ->
            do_reply(Name, Port, Status, #{}, Msg);
        noreply ->
            ok
    end,
    do_status(Name, idle, Msg).


do_reply(Name, Port, Status, NewMsg, Msg) ->
    lager:debug("Replying to ~p with status ~p and content ~p",
                [Port, Status, NewMsg]),

    NewMsg1 = case NewMsg of
                  #jup_msg{} -> NewMsg;
                  _ -> #jup_msg{content=NewMsg}
              end,

    NewMsg2 = #jup_msg{content=(NewMsg1#jup_msg.content)#{ status => Status }},

    Reply = jup_msg:add_headers(
              NewMsg2, Msg,
              to_reply_type(jup_msg:msg_type(Msg))
             ),

    jup_kernel_socket:send(Name, Port, Reply).


do_status(Name, Status, Parent) ->
    do_iopub(Name, status, #{ execution_state => Status }, Parent).


do_iopub(Name, MsgType, Msg, Parent) ->
    jup_kernel_iopub_srv:send(
      Name,
      jup_msg:add_headers(#jup_msg{content=Msg}, Parent, MsgType)
     ).


to_reply_type(MsgType) ->
    binary:replace(MsgType, <<"request">>, <<"reply">>).


do_process(Name, _Source, <<"kernel_info_request">>, Msg) ->
    {ok, Version} = file:read_file(
                      filename:join(
                        [
                         code:root_dir(),
                         "releases",
                         erlang:system_info(otp_release),
                         "OTP_VERSION"
                        ]
                       )
                     ),

    {KernelName, KernelVersion, KernelBanner} =
        jup_kernel_backend:kernel_info(Name, Msg),

    Content =
    #{
      protocol_version => <<"5.1">>,
      implementation => KernelName,
      implementation_version => KernelVersion,
      banner => KernelBanner,
      language_info => #{
        name => erlang,
        version => Version,
        file_extension => <<".erl">>
       }
     },

    {ok, Content};


do_process(Name, _Source, <<"execute_request">>, Msg) ->
    Content = Msg#jup_msg.content,
    Code = case maps:get(<<"code">>, Content) of
               <<"">> -> empty;
               Val -> Val
           end,

    do_iopub(
      Name, execute_input,
      #{
          code => Code,
          execution_count => jup_kernel_backend:exec_counter(Name)
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
    jup_kernel_backend:execute(Name, Code, Silent, StoreHistory, Msg);


do_process(Name, _Source, <<"is_complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,
    Code = maps:get(<<"code">>, Content),
    jup_kernel_backend:is_complete(Name, Code, Msg);


do_process(Name, _Source, <<"shutdown_request">>, _Msg) ->
    % Ignore restart for now
    jup_kernel_sup:stop(Name),
    noreply;


do_process(Name, _Source, <<"complete_request">>, Msg) ->
    Content = Msg#jup_msg.content,

    Code = maps:get(<<"code">>, Content),
    CursorPos = maps:get(<<"cursor_pos">>, Content),

    jup_kernel_backend:complete(Name, Code, CursorPos, Msg);


do_process(Name, Source, MsgType, Msg) ->
    lager:debug("Not implemented on ~s: ~p:~s~n~p", [Name, Source, MsgType,
                                                     lager:pr(Msg, ?MODULE)
                                                    ]
               ),

    {error, #{}}.
