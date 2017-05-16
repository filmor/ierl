-module(jup_kernel_protocol).

-include("internal.hrl").

-export([
         process_message/4
        ]).



process_message(Name, Port, MsgType, Msg) ->
    do_iopub(Name, #{ status => busy }),
    % TODO: Catch and send error?
    case do_process(Name, Port, MsgType, Msg) of
        {Status, Result} ->
            do_reply(Name, Port, Status, Result, Msg);
        Status when is_atom(Status) ->
            do_reply(Name, Port, Status, #{}, Msg);
        noreply ->
            ok
    end,
    do_iopub(Name, #{ status => idle }).


do_reply(Name, Port, Status, NewMsg, Msg) ->
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


do_iopub(Name, Msg) ->
    lager:info("[IOPUB ~p]: ~p", [Name, Msg]).


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
        jup_kernel_backend:kernel_info(Name),

    %    Build the proplist to be converted to json
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

    ok;


do_process(Name, _Source, <<"is_complete_request">>, Msg) ->
    case jup_kernel_backend:is_complete(Name, maps:get(<<"code">>, Msg)) of
        {incomplete, Indent} ->
            {incomplete, #{ indent => Indent }};
        Status when is_atom(Status) ->
            Status;
        _Else ->
            unknown
    end;


% TODO:
% do_process(Name, Source, <<"execute_request">>, Msg) ->
% do_process(Name, Source, <<"is_complete_request">>, Msg) ->
% do_process(Name, Source, <<"complete_request">>, Msg) ->
% do_process(Name, Source, <<"shutdown_request">>, Msg) ->
% do_process(Name, Source, <<"execute_request">>, Msg) ->

do_process(Name, Source, MsgType, Msg) ->
    lager:debug("Not implemented on ~s: ~p:~s~n~p", [Name, Source, MsgType,
                                                     lager:pr(Msg, ?MODULE)
                                                    ]
               ),

    {error, #{}}.
