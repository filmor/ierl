-module(jup_display).

-export([
    to_map/1,
    display/1,
    update/2
]).

-export_type([
    type/0
]).

-type type() ::
    data()
    | {mime_type(), data()}
    | [{mime_type(), data()}, ...]
    | #{mime_type() => data()}.

-type data() :: iodata().
-type mime_type() :: text | html | binary().

-spec to_map(Map :: type()) -> #{binary() => binary()}.
to_map(Bin) when is_binary(Bin) ->
    to_map(#{text => Bin});
to_map(Map) when is_map(Map) ->
    to_map(maps:to_list(Map));
to_map([H | T]) ->
    maps:merge(to_map(H), to_map(T));
to_map([]) ->
    #{};
to_map({MimeType, IOList}) when is_list(IOList); is_binary(IOList) ->
    MimeType2 = mimetype_to_binary(MimeType),
    #{MimeType2 => jup_util:ensure_binary(IOList)}.

-spec mimetype_to_binary(mime_type()) -> binary().
mimetype_to_binary(text) ->
    <<"text/plain">>;
mimetype_to_binary(html) ->
    <<"text/html">>;
mimetype_to_binary(Binary) when is_binary(Binary) ->
    Binary.

-spec display(type()) -> reference().
display(Obj) ->
    % Only works correctly when the io driver is jup_kernel_io
    io:request({jup_display, new, to_map(Obj)}).

-spec update(reference(), type()) -> ok.
update(Ref, Obj) ->
    io:request({jup_display, Ref, to_map(Obj)}).
