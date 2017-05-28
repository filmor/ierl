-module(ierl).

-export([main/1]).


-spec backends() -> #{}.
backends() ->
    #{
       erlang => ierl_backend_erlang
    }.


-spec commands() -> #{}.
commands() ->
    #{
        install => ierl_cmd_install,
        run => ierl_cmd_run
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
                    io:format("~p~n", [Specs]),
                    {"", Specs}
            end;
        _ ->
            {"", []}
    end.


do_list(Header, Map) ->
    io:format("~s:~n~n", [Header]),
    Names = maps:keys(Map),
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
    io:format("Usage: ~s <backend> <cmd>~n~n", [Cmd]),
    list_backends(),
    list_commands();


main([Backend | Rest]) ->
    BAtom = list_to_atom(Backend),
    case maps:find(BAtom, backends()) of
        {ok, Module} ->
            main({BAtom, Module}, Rest);
        _ ->
            main(["help"])
    end.


main(Backend, [Command | Rest]) ->
    CmdAtom = list_to_atom(Command),
    case maps:find(CmdAtom, commands()) of
        {ok, Module} ->
            main(Backend, {CmdAtom, Module}, Rest);
        _ ->
            main(["help"])
    end.


main({BAtom, Backend}, {CmdAtom, Command}, Rest) ->
    {_, Spec} = get_opts(Command),
    {_, BSpec} = get_opts(Backend),

    Spec1 = [{help, $h, "help", boolean, "This help text"}|Spec],

    {ok, {ParsedArgs, LeftOver}} = getopt:parse(Spec1, Rest),

    case proplists:get_value(help, ParsedArgs) of
        true ->
            getopt:usage(Spec1, io:format("ierl ~s ~s", [BAtom, CmdAtom]));
        _ ->
            Command:exec({BAtom, Backend}, ParsedArgs, LeftOver)
    end.
