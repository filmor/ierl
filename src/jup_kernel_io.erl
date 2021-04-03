-module(jup_kernel_io).

-include("internal.hrl").

-behaviour(gen_server).

-ignore_xref([
    start_link/1
]).

-export([
    start_link/1,
    get_pid/1
]).

-export([
    init/1,
    handle_info/2,
    handle_call/3,
    handle_cast/2,
    terminate/2,
    code_change/3
]).

-record(state, {
    name,
    opts = []
}).

-spec start_link(Name :: atom()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(?JUP_VIA(Name, io), ?MODULE, [Name], []).

-spec get_pid(Name :: atom()) -> pid().
get_pid(Name) ->
    gproc:where(?JUP_NAME(Name, io)).

init([Name]) ->
    {ok, #state{name = Name}}.

handle_info({io_request, From, ReplyAs, Request}, State) ->
    ?LOG_DEBUG("Got IO request: ~p", [Request]),
    {Result, State1} =
        try
            request(Request, State)
        catch
            error:Reason:Stacktrace ->
                ?LOG_DEBUG(
                    fun({R, S}) ->
                        FmtTrace = jup_util:format_stacktrace(R, S),
                        io_lib:format(
                            "Got IO Error ~p~n~s",
                            [R, FmtTrace]
                        )
                    end,
                    {Reason, Stacktrace}
                ),
                {{error, Reason}, State}
        end,

    ?LOG_DEBUG("Replying to IO request: ~p", [Result]),

    From ! {io_reply, ReplyAs, Result},
    {noreply, State1};
handle_info(_Msg, State) ->
    ?LOG_DEBUG("Got message ~p", [_Msg]),
    {noreply, State}.

handle_cast(_Cast, _State) ->
    error({invalid_cast, _Cast}).

handle_call(_Call, _From, _State) ->
    error({invalid_call, _Call}).

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    State.

request({put_chars, Encoding, Chars}, State) ->
    Res = put_chars(
        list_to_binary(unicode:characters_to_list(Chars, Encoding)),
        State
    ),
    {Res, State};
request({put_chars, Encoding, Module, Function, Args} = _L, State) ->
    ?LOG_DEBUG("Called put_chars as ~p", [_L]),
    request({put_chars, Encoding, apply(Module, Function, Args)}, State);
request({request, Requests}, State) when is_list(Requests) ->
    multi_request(Requests, {ok, State});
request({setopts, Opts}, State) ->
    {ok, State#state{opts = Opts}};
request(getopts, State) ->
    {{ok, State#state.opts}, State};
request({jup_display, RefOrNew, Map}, State) ->
    {display(RefOrNew, Map, State), State};
request(_Req, State) ->
    ?LOG_DEBUG("Unhandled IO request: ~p", [_Req]),
    {{error, request}, State}.

multi_request([R | Rs], {ok, State}) ->
    multi_request(Rs, request(R, State));
multi_request([R | Rs], {{ok, _}, State}) ->
    multi_request(Rs, request(R, State));
multi_request([_ | _], {Error, State}) ->
    {Error, State};
multi_request([], Result) ->
    Result.

put_chars(Chars, State) ->
    case proplists:get_value(jup_msg, State#state.opts) of
        undefined ->
            {error, no_jup_msg_found};
        Msg ->
            Content = #{name => stdout, text => Chars},

            Msg1 = jup_msg:add_headers(
                #jup_msg{content = Content},
                Msg,
                stream
            ),

            jup_kernel_iopub_srv:send(State#state.name, Msg1),

            ok
    end.

display(new, Map, State) ->
    Ref = jup_util:get_uuid(),
    case display(Ref, Map, <<"display_data">>, State) of
        ok ->
            {ok, Ref};
        Else ->
            Else
    end;
display(Ref, Map, State) ->
    display(Ref, Map, <<"update_display_data">>, State).

display(Ref, Map, MsgType, State) ->
    case proplists:get_value(jup_msg, State#state.opts) of
        undefined ->
            ?LOG_DEBUG("~p", [Map]),
            {error, no_jup_msg_found};
        Msg ->
            Content = #{
                data => Map,
                metadata => #{},
                transient => #{display_id => Ref}
            },

            Msg1 = jup_msg:add_headers(
                #jup_msg{content = Content},
                Msg,
                MsgType
            ),

            jup_kernel_iopub_srv:send(State#state.name, Msg1),

            ok
    end.
