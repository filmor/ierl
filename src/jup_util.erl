-module(jup_util).

-export([
         split_at_delim/2,
         ensure_binary/1,
         hexlify/1,

         call_if_exported/3,
         call_if_exported/4,

         copy_to_node/2
        ]).


-spec ensure_binary(string() | atom() | binary()) -> binary().
ensure_binary(List) when is_list(List) ->
    list_to_binary(List);

ensure_binary(Atom) when is_atom(Atom) ->
    atom_to_binary(Atom, utf8);

ensure_binary(Binary) when is_binary(Binary) ->
    Binary.


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


-spec call_if_exported(module(), atom(), list()) -> term().
call_if_exported(Module, Func, Args) ->
    call_if_exported(Module, Func, Args, undefined).


-spec call_if_exported(module(), atom(), list(), term()) -> term().
call_if_exported(Module, Func, Args, Default) ->
    case erlang:function_exported(Module, Func, length(Args)) of
        true ->
            Module:Func(Args);
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
            do_copy_to_node(Node, [jup_kernel_worker, Backend] ++ Deps)
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
