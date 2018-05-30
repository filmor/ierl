-module(jup_kernel_dispatcher).

-behaviour(gen_server).

-include("internal.hrl").

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


-define(QUEUES, [control, shell]).

-type task() :: any().


-record(state, {
          name    :: jupyter:name(),
          queues  :: #{ jupyter:port() => queue:queue() },
          current :: task(),
          current_pid :: pid()
         }
       ).


-spec start_link(jupyter:name()) -> {ok, pid()}.
start_link(Name) ->
    gen_server:start_link(?JUP_VIA(Name, dispatcher), ?MODULE, [Name], []).


-spec push(jupyter:name(), jupyter:port(), jup_msg:type()) -> ok.
push(Name, Port, Msg) ->
    gen_server:cast(?JUP_VIA(Name, dispatcher), {push, Port, Msg}).


-spec flush(jupyter:name()) -> ok.
flush(Name) ->
    gen_server:cast(?JUP_VIA(Name, dispatcher), flush).


init([Name]) ->
    % TODO Implement stdin support
    Queues = maps:from_list(
               [{Port, queue:new()} || Port <- ?QUEUES]
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

    {noreply, do_process(State1)};


handle_cast(flush, State) ->
    Q = maps:map(fun (_, _) -> queue:new() end, State#state.queues),
    State1 = State#state{queues=Q},
    {noreply, State1}.


handle_info(wake_up, State) ->
    {noreply, do_process(State)}.


terminate(_Reason, _State) ->
    ok.


code_change(_OldVsn, State, _Extra) ->
    State.


do_process(State) when State#state.current =:= undefined ->
    {Res, NewQueues} = get_first(?QUEUES, State#state.queues),

    State1 = State#state{queues=NewQueues},

    State2 =
    case Res of
        {value, Queue, Value} ->
            % Do something with it
            lager:debug("Dequeued ~p from ~p", [Value, Queue]),
            State1#state{current=Value};
        empty ->
            lager:debug("Nothing to dequeue"),
            State1
    end,

    State2;

do_process(State) ->
    State.


get_first([], Queues) ->
    {empty, Queues};

get_first([Name | Names], Queues) ->
    case queue:out(maps:get(Name, Queues)) of
        {empty, _Q} ->
            get_first(Names, Queues);
        {{value, Val}, Q} ->
            {{value, Name, Val}, Queues#{ Name => Q} }
    end.
