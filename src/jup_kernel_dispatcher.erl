-module(jup_kernel_dispatch).

-behaviour(gen_server).

-export([
         start_link/1,
         push/3,
         flush/1
        ]).

-export([
         init/1,
         handle_info/2,
         handle_call/3,
         handle_cast/2,
         terminate/2,
         code_change/3
        ]).

-record(state, [
                name    :: jupyter:name(),
                queues  :: #{ jupyter:port() => queue:queue() }
               ]
       ).


-spec start_link(jupyter:name()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(?JUP_VIA(Name, dispatcher), ?MODULE, [Name], []).


-spec push(jupyter:name(), jupyter:port(), jup_msg:type()) -> ok.
push(Name, Port, Msg) ->
    gen_server:cast(?JUP_VIA(Name, dispatcher), {push, Port, Msg}).


init([Name]) ->
    % TODO Implement stdin support
    Queues = maps:from_list(
               [{Port, queue:new()} || Port <- [shell, control]]
              ),

    State = #state{
               name = Name,
               queues = Queues,
               current = undefined,
               current_pid = undefined
              },

    {ok, State}.


handle_call(_Call, _From, _State) ->
    error({invalid_call, _Call}).


handle_cast({push, Port, Msg}, State) ->
    Queues = State#state.queues,
    Q0 = maps:get(Port, State#state.queues),
    Q1 = queue:in(Msg, Q0),

    State1 = State#state{queues=Queues#{ Port => Q1 }},

    {noreply, do_process(State1)}.


handle_info(wake_up, State) ->
    {noreply, do_process(State)}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    State.


do_process(State#state{current = undefined}) ->
    % Check control queue first
    ;

do_process(State) ->
    State.
