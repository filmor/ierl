-module(jup_util).

-export([
         split_at_delim/2,
         ensure_binary/1,
         hexlify/1
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
