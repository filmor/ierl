-module(ierl_backend_elixir).

-behaviour(jup_kernel_backend).


-export([
         init/1,
         deps/0,
         do_kernel_info/2,
         do_execute/3,
         do_is_complete/3,
         do_complete/4,
         opt_spec/0,
         language/0
        ]).


-record(state, {
          bindings = [],
          modules
         }).


opt_spec() ->
    {
     "Simple Elixir backend",
     [
      {path, undefined, "elixir-path", string, "Elixir install root directory"}
     ]
    }.


language() ->
    elixir.


init(Args) ->
    ElixirPath = get_elixir_path(Args),
    ElixirAppPath = filename:join([ElixirPath, elixir, ebin]),
    IExAppPath = filename:join([ElixirPath, iex, ebin]),

    code:add_path(ElixirAppPath),
    code:add_path(IExAppPath),

    {ok, _} = application:ensure_all_started(elixir),

    % We have to make sure that the compiler is loaded, otherwise the initial
    % is_complete call will answer too slowly and Jupyter will think it was not
    % implemented.
    'Elixir.Code':string_to_quoted(":undefined"),

    #state{}.


deps() ->
    [ierl_versions, ierl_util].


do_kernel_info(_Msg, State) ->
    Content =
    #{
      implementation => ?MODULE,
      implementation_version => ierl_versions:get_app_version(ierl),
      banner => <<"Elixir Jupyter Kernel">>,
      language_info => #{
        name => elixir,
        version => ierl_versions:get_app_version(elixir),
        file_extension => ex,
        codemirror_mode => ruby
       }
     },

    {Content, State}.


do_execute(Code, _Msg, State) ->
    try
        {Res, NewBindings} =
            'Elixir.Code':eval_string(Code, State#state.bindings),

        Res1 = 'Elixir.Kernel':inspect(Res),

        {{ok, Res1}, State#state{bindings=NewBindings}}
    catch
        Type:Error ->
            Normalized = 'Elixir.Exception':normalize(Type, Error),
            Msg = 'Elixir.Exception':message(Normalized),
            Formatted = 'Elixir.Exception':format(Type, Normalized),

            {{error, Type, Msg, [Formatted]}, State}
    end.


do_is_complete(Code, _Msg, State) ->
    Res = try
              'Elixir.Code':'string_to_quoted!'(Code),
              complete
          catch
              error:#{ '__struct__' := 'Elixir.TokenMissingError'} ->
                  incomplete;
              error:_Other ->
                  invalid
          end,

    {Res, State}.


do_complete(Code, CursorPos, _Msg, State) ->
    L = lists:sublist(binary_to_list(Code), CursorPos),
    Res = case 'Elixir.IEx.Autocomplete':expand(lists:reverse(L)) of
              {yes, Expansion, []} ->
                  [Expansion];
              {yes, [], Matches} ->
                  [
                   Name ||
                   {Name, _Arity} <- lists:map(fun split_arity/1, Matches)
                  ];
              {no, [], Matches} ->
                  [
                   Name ||
                   {Name, _Arity} <- lists:map(fun split_arity/1, Matches)
                  ]
          end,

    {[list_to_binary(R) || R <- Res], State}.


split_arity(Str) ->
    {Name, ArityPart} = lists:splitwith(fun (X) -> X =/= $/ end, Str),

    Arity = case ArityPart of
                [$/ | Rest] ->
                    list_to_integer(Rest);
                _ ->
                    undefined
            end,

    {Name, Arity}.


get_elixir_path(Args) ->
    case maps:find(path, Args) of
        error ->
            ExPath = os:cmd("elixir -e \"IO.write(:code.lib_dir(:elixir))\""),
            ierl_util:simplify(filename:join(ExPath, ".."));
        {ok, Value} ->
            Value
    end.
