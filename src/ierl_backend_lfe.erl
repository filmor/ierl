-module(ierl_backend_lfe).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         do_execute/4,
         do_is_complete/3,
         do_complete/4,
         opt_spec/0,
         language/0
        ]).


-record(state, {
          env
         }).


opt_spec() ->
    {
     "Simple LFE backend",
     []
    }.


language() ->
    'common-lisp'.


init(_Args) ->
    {ok, _} = application:ensure_all_started(lfe),
    #state{env=lfe_env:new()}.


do_execute(Code, _Publish, _Msg, State) ->
    try
        {Res, NewState} =
            lfe_shell:run_string(binary_to_list(Code), State#state.env),

        NewBindings = element(2, NewState),
        Res1 = jup_util:ensure_binary(lfe_io_pretty:term(Res)),

        {{ok, Res1}, State#state{env=NewBindings}}
    catch
        Type:Reason ->
            Stacktrace = format_stacktrace(erlang:get_stacktrace()),
            Reason1 = jup_util:ensure_binary(io_lib:format("~p", [Reason])),

            {{error, Type, Reason1, Stacktrace}, State}
    end.


do_is_complete(Code, _Msg, State) ->
    Res = case lfe_scan:string(binary_to_list(Code), 1) of
              {ok, Tokens, _} ->
                  case lfe_parse:sexpr(Tokens) of
                      {more, _} ->
                          incomplete;
                      {ok, _, _, _} ->
                          complete;
                      _ ->
                          invalid
                  end;

              _ ->
                  invalid
          end,

    {Res, State}.


do_complete(Code, CursorPos, _Msg, State) ->
    L = lists:sublist(binary_to_list(Code), CursorPos),
    Res = case lfe_edlin_expand:expand(lists:reverse(L)) of
              {yes, Expansion, []} ->
                  [Expansion];
              {yes, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches];
              {no, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches]
          end,

    {[list_to_binary(R) || R <- Res], State}.


format_stacktrace(Stacktrace) ->
    % Shamelessly stolen directly from lfe_shell
    %
    Sf = fun ({M,_F,_A,_L}) ->
                 %% Don't want to see these in stacktrace.
                 (M == lfe_eval) or (M == ?MODULE)
         end,

    Ff = fun (T, I) -> lfe_io:prettyprint1(T, 15, I, 80) end,

    IoList = lfe_lib:format_stacktrace(Stacktrace, Sf, Ff),
    [jup_util:ensure_binary(IoList)].