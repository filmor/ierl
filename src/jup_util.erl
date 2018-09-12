-module(jup_util).

-export([
         split_at_delim/2,
         ensure_binary/1,
         ensure_string/1,
         hexlify/1,

         get_uuid/0,
         get_user/0,

         call_if_exported/3,
         call_if_exported/4,

         copy_to_node/2,

         unique/1
        ]).


-export_type([string_like/0]).

-type string_like() :: string() | atom() | binary() | iolist().


-spec ensure_binary(string_like()) -> binary().
ensure_binary(List) when is_list(List) ->
    list_to_binary(List);

ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

ensure_binary(Binary) when is_binary(Binary) ->
    Binary.


-spec ensure_string(string_like()) -> string().
ensure_string(List) when is_list(List) ->
    lists:flatten(List);

ensure_string(Atom) when is_atom(Atom) ->
    atom_to_list(Atom);

ensure_string(Binary) when is_binary(Binary) ->
    binary_to_list(Binary).


% Helper function: Split a list at a given delimiter, fail if the delimiter
% cannot be found.
-spec split_at_delim(list(), any()) -> {list(), list()}.
split_at_delim(List, Delim) ->
    split_at_delim1(List, Delim, []).


split_at_delim1([Delim|Tail], Delim, Prefix) ->
    {lists:reverse(Prefix), Tail};

split_at_delim1([Head|Tail], Delim, Prefix) ->
    split_at_delim1(Tail, Delim, [Head|Prefix]).


-spec hexlify(binary()) -> binary().
hexlify(Bin) when is_binary(Bin) ->
    << <<(hex(H)),(hex(L))>> || <<H:4,L:4>> <= Bin >>.

hex(C) when C < 10 -> $0 + C;
hex(C) -> $a + C - 10.


-spec get_uuid() -> binary().
get_uuid() ->
    jup_util:ensure_binary(uuid:uuid_to_string(uuid:get_v4())).


-spec get_user() -> binary().
get_user() ->
    L =
    [X ||
     X <- [os:getenv(X) || X <- ["USERNAME", "USER", "LOGNAME"]],
     X =/= false,
     X =/= ""
    ],

    case L of
        [Head|_] ->
            ensure_binary(Head);
        _ ->
            <<"username">>
    end.


-spec call_if_exported(module(), atom(), list()) -> term().
call_if_exported(Module, Func, Args) ->
    call_if_exported(Module, Func, Args, undefined).


-spec call_if_exported(module(), atom(), list(), term()) -> term().
call_if_exported(Module, Func, Args, Default) ->
    case erlang:function_exported(Module, Func, length(Args)) of
        true ->
            erlang:apply(Module, Func, Args);
        _ ->
            Default
    end.


-spec copy_to_node(atom(), module()) -> ok | {error, [{module(), term()}]}.
copy_to_node(Node, Backend) ->
    Deps = jup_util:call_if_exported(Backend, deps, [], []),

    case Node =:= node() of
        true ->
            code:ensure_modules_loaded([Backend] ++ Deps);
        _ ->
            Deps1 =
            [
             jup_kernel_worker, jup_kernel_protocol, jup_display, jup_util,
             jup_msg, Backend
            ] ++ Deps,

            lager:debug("Copying ~p to ~p", [Node, Deps1]),

            do_copy_to_node(Node, Deps1)
    end.


do_copy_to_node(Node, Modules) ->
    Errs =
    lists:foldl(
      fun (Module, Errors) ->
              % Never override remote modules
              case rpc:call(Node, code, is_loaded, [Module]) of
                  {file, _} ->
                      ok;
                  _ ->
                      case code:get_object_code(Module) of
                          error ->
                              [{Module, no_object_code} | Errors];
                          {_, Bin, Fn} ->
                              case rpc:call(
                                     Node, code, load_binary,
                                     [Module, Fn, Bin]
                                    ) of
                                  {module, _} ->
                                      Errors;
                                  {error, Error} ->
                                      [{Module, Error} | Errors]
                              end
                      end
              end
      end,
      [],
      Modules
     ),

    case Errs of
        [] ->
            ok;
        _ ->
            {error, Errs}
    end.


% @doc Make a list contain sorted unique elements
-spec unique(list()) -> list().
unique(L) ->
    lists:sort(sets:to_list(sets:from_list(L))).
