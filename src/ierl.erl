-module(ierl).

-export([main/1]).


-spec backends() -> map().
backends() ->
    #{
       erlang => ierl_backend_erlang,
       elixir => ierl_backend_elixir,
       lfe => ierl_backend_lfe
    }.


-spec commands() -> map().
commands() ->
    #{
        % Install a kernel permanently under a given name
        install => ierl_cmd_install,
        % Execute a kernel using a connection file
        kernel => ierl_cmd_kernel
    }.


-spec get_opts(Mod :: module()) -> {string(), [getopt:option_spec()]}.
get_opts(Mod) ->
    {module, _} = code:ensure_loaded(Mod),

    case erlang:function_exported(Mod, opt_spec, 0) of
        true ->
            case Mod:opt_spec() of
                {Usage, Specs} ->
                    {Usage, Specs};
                Specs ->
                    {"", Specs}
            end;
        _ ->
            {"", []}
    end.


do_list(Header, Map) ->
    io:format("~s:~n~n", [Header]),
    Lines = lists:sort(
              maps:fold(
                fun (K, V, Res) ->
                        [{atom_to_list(K), element(1, get_opts(V))} | Res]
                end,
                [],
                Map
               )
             ),

    lists:foreach(
      fun ({Name, Usage}) ->
              io:format(" ~-30s ~s~n", [Name, Usage])
      end,
      Lines
     ),
    io:format("~n").


-spec list_commands() -> ok.
list_commands() ->
    do_list("Available commands", commands()).

-spec list_backends() -> ok.
list_backends() ->
    do_list("Available backends", backends()).


-spec main([string()]) -> term().
main([]) ->
    % Print usage
    main(["help"]);

main(["--help"]) ->
    main(["help"]);

main(["help"]) ->
    Cmd = filename:basename(escript:script_name()),
    io:format("Usage: ~s <cmd> <backend>~n~n", [Cmd]),
    list_commands(),
    list_backends();

main([Command | Rest]) ->
    CmdAtom = list_to_atom(Command),
    case maps:find(CmdAtom, commands()) of
        {ok, Module} ->
            main({CmdAtom, Module}, Rest);
        _ ->
            main(["help"])
    end.


main(Command, []) ->
    main(Command, ["erlang"]);

main(Command, [Backend | Rest]) ->
    {BAtom, Rest1} = case Backend of
                         [$-|_] ->
                             % Fall back to "erlang" as the default backend
                             {erlang, [Backend | Rest]};
                         _ ->
                             {list_to_atom(Backend), Rest}
                     end,

    case maps:find(BAtom, backends()) of
        {ok, Module} ->
            main(Command, {BAtom, Module}, Rest1);
        _ ->
            main(["help"])
    end.


main({CmdAtom, Command}, {BAtom, Backend}, Rest) ->
    % TODO Replace lager for normal commands, only use it for the actual kernel
    application:load(lager),

    application:set_env(lager, suppress_application_start_stop, true),
    application:set_env(lager, suppress_supervisor_start_stop, true),

    {ok, _} = application:ensure_all_started(lager),
    lager:set_loglevel(lager_console_backend, debug),

    {_, Spec} = get_opts(Command),
    {_, _BSpec} = get_opts(Backend),

    Spec1 = [{help, $h, "help", boolean, "This help text"}|Spec],

    {ok, {ParsedArgs, LeftOver}} = getopt:parse(Spec1, Rest),

    case proplists:get_value(help, ParsedArgs) of
        true ->
            getopt:usage(
              Spec1, lists:flatten(io_lib:format("ierl ~s ~s",
                                                 [BAtom, CmdAtom])
                                  )
             );
        _ ->
            Command:exec({BAtom, Backend}, ParsedArgs, LeftOver)
    end.
