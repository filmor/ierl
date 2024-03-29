-module(ierl_backend_elixir).

% Ignore all Elixir functions as xref has no way to find out about them
-ignore_xref([
    {'Elixir.IEx.Autocomplete', expand, 1},
    {'Elixir.Exception', format, 2},
    {'Elixir.Exception', message, 1},
    {'Elixir.Exception', normalize, 2},
    {'Elixir.Kernel', inspect, 1},
    {'Elixir.Code', string_to_quoted, 1},
    {'Elixir.Code', 'string_to_quoted!', 1},
    {'Elixir.Code', eval_string, 2}
]).

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
    complete/4,
    inspect/5
]).

-record(state, {
    bindings = [],
    modules,
    counter = 0
}).

opt_spec() ->
    {
        "Simple Elixir backend",
        [
            {path, undefined, "elixir-path", string, "Elixir install root directory"}
        ]
    }.

language() ->
    <<"elixir">>.

init(Args) ->
    case code:ensure_loaded(elixir) of
        {module, _} ->
            % elixir is already in the code path, assume we're running embedded
            ok;
        _ ->
            ElixirPath = get_elixir_path(Args),
            ElixirAppPath = filename:join([ElixirPath, elixir, ebin]),
            IExAppPath = filename:join([ElixirPath, iex, ebin]),

            code:add_path(ElixirAppPath),
            code:add_path(IExAppPath)
    end,

    {ok, _} = application:ensure_all_started(elixir),

    % We have to make sure that the compiler is loaded, otherwise the initial
    % is_complete call will answer too slowly and Jupyter will think it was not
    % implemented.
    'Elixir.Code':string_to_quoted(":undefined"),

    #state{}.

deps() ->
    [ierl_versions, ierl_util].

kernel_info(_Msg, _State) ->
    Content =
        #{
            implementation => ?MODULE,
            implementation_version => ierl_versions:get_app_version(ierl),
            banner => <<"Elixir Jupyter Kernel">>,
            language_info => #{
                name => elixir,
                version => ierl_versions:get_app_version(elixir),
                file_extension => <<".ex">>,
                codemirror_mode => ruby
            }
        },

    Content.

execute(Code, _Msg, State) ->
    Counter = State#state.counter,
    try
        {Res, NewBindings} =
            'Elixir.Code':eval_string(Code, State#state.bindings),

        Res1 = 'Elixir.Kernel':inspect(Res),
        Counter1 = Counter + 1,

        {
            {ok, Res1},
            Counter1,
            State#state{bindings = NewBindings, counter = Counter1}
        }
    catch
        Type:Error ->
            Normalized = 'Elixir.Exception':normalize(Type, Error),
            Msg = 'Elixir.Exception':message(Normalized),
            Formatted = 'Elixir.Exception':format(Type, Normalized),

            {{error, Type, Msg, [Formatted]}, Counter, State}
    end.

exec_counter(State) ->
    State#state.counter.

is_complete(Code, _Msg, _State) ->
    try
        'Elixir.Code':'string_to_quoted!'(Code),
        complete
    catch
        error:#{'__struct__' := 'Elixir.TokenMissingError'} ->
            incomplete;
        error:_Other ->
            invalid
    end.

complete(Code, CursorPos, _Msg, _State) ->
    % TODO: Match in the environment
    Code1 = binary_to_list(Code),
    L = lists:sublist(Code1, CursorPos),
    case 'Elixir.IEx.Autocomplete':expand(lists:reverse(L)) of
        {yes, Expansion, []} ->
            [Expansion];
        {yes, [], Matches} ->
            {Start, End} = ierl_util:find_span(Code1, CursorPos),
            {Start, End, [
                Name
             || {Name, _Arity} <- lists:map(fun split_arity/1, Matches)
            ]};
        {no, [], _} ->
            []
    end.

inspect(_Code, _CursorPos, _Detail, _Msg, _State) ->
    not_found.

split_arity(Str) ->
    {Name, ArityPart} = lists:splitwith(fun(X) -> X =/= $/ end, Str),

    Arity =
        case ArityPart of
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
