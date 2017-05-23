-module(jup_kernel_backend_stub).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         do_execute/4,
         do_is_complete/3,
         do_complete/4
        ]).


-record(state, {
          bindings
         }).


init(_Args) ->
    #state{
       bindings=erl_eval:new_bindings()
      }.


do_execute(Code, _Publish, _Msg, State) ->
    try
        {Value, Bindings} = evaluate(binary_to_list(Code),
                                     State#state.bindings),

        {{ok, Value}, State#state{bindings=Bindings}}
    catch
        Type:Reason ->
            lager:error("Error: ~p:~p", [Type, Reason]),
            Stacktrace = [
                          list_to_binary(io_lib:format("~p", [Item])) ||
                          Item <- erlang:get_stacktrace()
                         ],
            Reason1 = list_to_binary(
                        io_lib:format("~p", [Reason])
                       ),

            {{error, Type, Reason1, Stacktrace}, State}
    end.


do_complete(Code, CursorPos, _Msg, State) ->
    L = lists:sublist(binary_to_list(Code), CursorPos),
    Res = case edlin_expand:expand(lists:reverse(L)) of
              {yes, Expansion, []} ->
                  [Expansion];
              {yes, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches];
              {no, [], Matches} ->
                  [Name || {Name, _Arity} <- Matches]
          end,

    {[list_to_binary(R) || R <- Res], State}.


do_is_complete(Code, _Msg, State) ->
    Res = case erl_scan:string(binary_to_list(Code)) of
              {ok, Tokens, _} ->
                  check_is_complete(Tokens, [dot]);
              _ ->
                  invalid
          end,

    {Res, State}.


evaluate(Expression, Bindings) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, Bindings1} = erl_eval:exprs(Parsed, Bindings),
    {Result, Bindings1}.


check_is_complete([], []) ->
    complete;

check_is_complete([], _List) ->
    {incomplete, <<"...">>};

check_is_complete([{Token, _}|Tail], [Token|Stack]) ->
    check_is_complete(Tail, Stack);

check_is_complete([{_ValueToken, _, _}|Tail], Stack) ->
    check_is_complete(Tail, Stack);

check_is_complete([{Token, _}|Tail], Stack) ->
    Add = case Token of
              'fun' -> 'end';
              'case' -> 'end';
              'if' -> 'end';
              'receive' -> 'end';
              'try' -> 'end';
              'begin' -> 'end';

              '<<' -> '>>';

              '(' -> ')';
              '[' -> ']';
              '{' -> '}';

              'end' -> invalid;
              ')' -> invalid;
              ']' -> invalid;
              '}' -> invalid;
              '>>' -> invalid;

              _ ->
                  none
          end,

    case Add of
        none ->
            check_is_complete(Tail, Stack);
        invalid ->
            invalid;
        _ ->
            check_is_complete(Tail, [Add|Stack])
    end.
