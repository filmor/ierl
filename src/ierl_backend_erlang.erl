-module(ierl_backend_erlang).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         do_kernel_info/2,
         do_execute/3,
         do_is_complete/3,
         do_complete/4,
         opt_spec/0
        ]).


-record(state, {
          bindings,
          modules
         }).


opt_spec() ->
    {
     "Simple Erlang backend",
     []
    }.


init(_Args) ->
    #state{
       bindings=erl_eval:new_bindings()
      }.


do_kernel_info(_Msg, State) ->
    Content =
    #{
      implementation => ?MODULE,
      implementation_version => ierl_versions:get_app_version(ierl),
      banner => <<"Erlang Jupyter Kernel">>,
      language_info => #{
        name => erlang,
        version => ierl_versions:get_otp_version(),
        file_extension => erl
       }
     },

    {Content, State}.


do_execute(Code, _Msg, State) ->
    try
        {Value, State1} = evaluate(binary_to_list(Code), State),
        Str = iolist_to_binary(io_lib:format("~p~n", [Value])),
        {{ok, Str}, State1}
    catch
        Type:Reason ->
            lager:debug("Error: ~p:~p", [Type, Reason]),
            Stacktrace = lager:pr_stacktrace(
                           erlang:get_stacktrace()
                          ),

            Reason1 = list_to_binary(
                        io_lib:format("~p", [Reason])
                       ),

            St = [
                  io_lib:format("~s:~s~n~nStacktrace:", [Type, Reason1]),
                  Stacktrace
                 ],

            {{error, Type, Reason1, St}, State}
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
    % TODO: Check if module is complete by looking for two empty lines at the
    % end?
    Res = case erl_scan:string(binary_to_list(Code)) of
              {ok, Tokens, _} ->
                  check_is_complete(Tokens, [dot]);
              _ ->
                  invalid
          end,

    {Res, State}.


evaluate(Expression, State) ->
    {ok, Tokens, _} = erl_scan:string(Expression),

    case is_module(Tokens) of
        false ->
            {ok, Parsed} = erl_parse:parse_exprs(Tokens),
            {value, Result, Bindings1} = erl_eval:exprs(Parsed,
                                                        State#state.bindings),
            {Result, State#state{bindings=Bindings1}};
        ModuleName ->
            {compile_module(ModuleName, Tokens, State), State}
    end.


compile_module(ModuleName, Tokens, _State) ->
    % TODO: Test that the module can currently not be loaded or has been loaded
    % through this mechanism before.
    FormGroups = lists:foldr(
                   fun ({dot, _} = Token, Acc) ->
                           [[Token] | Acc];
                       (Token, [H | T]) ->
                           [[Token | H] | T]
                   end,
                   [],
                   Tokens
                  ),

    ParseRes = [erl_parse:parse_form(Tokens1) || Tokens1 <- FormGroups],

    Errors = [Err || {error, Err} <- ParseRes],

    case Errors of
        [] ->
            Forms = [Form || {ok, Form} <- ParseRes],

            case compile:forms(Forms) of
                {ok, ModuleName, Code} ->
                    % TODO: Filename = "jupyter_{name}_{execution_count}"
                    {module, _} = code:load_binary(ModuleName, "jupyter", Code);
                Res ->
                    Res
            end;

        Errors ->
            {error, Errors}
    end.


% Checks if the given token stream is a module
-spec is_module(list()) -> boolean().
is_module(
 [{'-', _}, {atom, _, module}, {'(', _}, {atom, _, Module}, {')', _}, {dot, _} |
  _Rest]
 ) ->
    Module;
is_module(_List) ->
    false.


check_is_complete([], []) ->
    complete;

check_is_complete([], List) ->
    {incomplete, << <<"  ">> || _ <- List >>};

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
