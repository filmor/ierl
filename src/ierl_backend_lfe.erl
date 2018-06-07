-module(ierl_backend_lfe).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         deps/0,
         opt_spec/0,
         language/0,

         kernel_info/2,
         execute/3,
         exec_counter/1,
         is_complete/3,
         complete/4
        ]).


-record(state, {
          env,
          counter = 0
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


deps() ->
    [ierl_versions].


kernel_info(_Msg, _State) ->
    Content =
    #{
      implementation => ?MODULE,
      implementation_version => ierl_versions:get_app_version(ierl),
      banner => <<"LFE Jupyter Kernel">>,
      language_info => #{
        name => lfe,
        version => ierl_versions:get_app_version(lfe),
        file_extension => <<".lfe">>,
        codemirror_mode => <<"commonlisp">>,
        pygments_lexer => <<"common-lisp">>
       }
     },

    Content.


execute(Code, _Msg, State) ->
    Counter = State#state.counter,
    try
        {Res, NewState} =
            lfe_shell:run_string(binary_to_list(Code), State#state.env),

        NewBindings = element(2, NewState),
        Res1 = jup_util:ensure_binary(lfe_io_pretty:term(Res)),

        Counter1 = Counter + 1,

        {{ok, Res1}, Counter1, State#state{env=NewBindings, counter=Counter1}}
    catch
        Type:Reason ->
            Stacktrace = format_stacktrace(erlang:get_stacktrace()),
            Reason1 = jup_util:ensure_binary(io_lib:format("~p", [Reason])),

            St = [
                  io_lib:format("~s:~s~n~nStacktrace:~n", [Type, Reason1])
                  | Stacktrace
                 ],

            {{error, Type, Reason1, St}, Counter, State}
    end.


exec_counter(State) ->
    State#state.counter.


is_complete(Code, _Msg, _State) ->
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

    Res.


complete(Code, CursorPos, _Msg, State) ->
    % TODO: Check in the environment for completables
    _Env = State#state.env,
    L = lists:sublist(binary_to_list(Code), CursorPos),
    Res = case lfe_edlin_expand:expand(lists:reverse(L)) of
              {yes, Expansion, []} ->
                  [Expansion];
              {yes, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches];
              {no, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches]
          end,

    [list_to_binary(R) || R <- Res].


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