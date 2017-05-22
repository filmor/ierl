-module(jup_kernel_backend_stub).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         do_execute/4,
         do_is_complete/3
        ]).


-record(state, {
          bindings
         }).


init(_Args) ->
    #state{
       bindings=erl_eval:new_bindings()
      }.


do_execute(Code, _Publish, _Msg, State) ->
    Res = try
              {ok, evaluate(binary_to_list(Code))}
          catch
              Type:Reason ->
                  lager:error("Error: ~p:~p", [Type, Reason]),
                  Stacktrace = [
                                list_to_binary(io_lib:format("~p", [Item])) ||
                                Item <- erlang:get_stacktrace()
                               ],
                  {error, Type, Reason, Stacktrace}
          end,

    {Res, State}.


% do_complete(Code
% use edlin_expand:expand(lists:reverse(binary_to_list(Code)))

do_is_complete(Code, _Msg, State) ->
    Res = case erl_scan:string(binary_to_list(Code)) of
              {ok, Tokens, _} ->
                  check_is_complete(Tokens, [dot]);
              _ ->
                  invalid
          end,

    {Res, State}.


evaluate(Expression) ->
    {ok, Tokens, _} = erl_scan:string(Expression),
    {ok, Parsed} = erl_parse:parse_exprs(Tokens),
    {value, Result, _} = erl_eval:exprs(Parsed, []),
    Result.


check_is_complete([], []) ->
    complete;

check_is_complete([], _List) ->
    incomplete;

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
