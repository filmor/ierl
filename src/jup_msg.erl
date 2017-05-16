-module(jup_msg).

-export([
         decode/2,
         encode/2,
         msg_type/1,
         add_headers/3
        ]).

-include("internal.hrl").

-define(DELIM, <<"<IDS|MSG>">>).
-define(VERSION, <<"5.1">>).


-spec decode([binary()], {crypto:hash_algorithms(), binary()}) -> #jup_msg{}.
decode(MultipartMsg, {SignatureScheme, Key}) ->
    {Uuids, Suffix} = jup_util:split_at_delim(MultipartMsg, ?DELIM),

    [HMAC, Header, ParentHeader, Metadata, Content | ExtraBinaries] = Suffix,

    Ctx0 = crypto:hmac_init(SignatureScheme, Key),
    Ctx1 = crypto:hmac_update(Ctx0, Header),
    Ctx2 = crypto:hmac_update(Ctx1, ParentHeader),
    Ctx3 = crypto:hmac_update(Ctx2, Metadata),
    Ctx4 = crypto:hmac_update(Ctx3, Content),

    Signature = jup_util:hexlify(crypto:hmac_final(Ctx4)),

    case Signature of
        HMAC -> ok;
        _ -> error(invalid_signature)
    end,

    #jup_msg{
       uuids = Uuids,
       header = jsx:decode(Header, [return_maps]),
       parent_header = jsx:decode(ParentHeader, [return_maps]),
       metadata = jsx:decode(Metadata, [return_maps]),
       content = jsx:decode(Content, [return_maps]),

       extra_binaries = ExtraBinaries
      }.


-spec encode(#jup_msg{}, {crypto:hash_algorithms(), binary()}) -> [binary()].
encode(#jup_msg{} = Msg, {SignatureScheme, Key}) ->
    Ctx0 = crypto:hmac_init(SignatureScheme, Key),
    Header = jsx:encode(Msg#jup_msg.header),
    Ctx1 = crypto:hmac_update(Ctx0, Header),
    ParentHeader = jsx:encode(Msg#jup_msg.parent_header),
    Ctx2 = crypto:hmac_update(Ctx1, ParentHeader),
    Metadata = jsx:encode(Msg#jup_msg.metadata),
    Ctx3 = crypto:hmac_update(Ctx2, Metadata),
    Content = jsx:encode(Msg#jup_msg.content),
    Ctx4 = crypto:hmac_update(Ctx3, Content),

    Signature = jup_util:hexlify(crypto:hmac_final(Ctx4)),

    % TODO
    Msg#jup_msg.uuids ++ [
     ?DELIM,
     Signature,
     Header,
     ParentHeader,
     Metadata,
     Content
     | Msg#jup_msg.extra_binaries
    ].


header_entry(#jup_msg{header=Header}, Key) ->
    BinKey = jup_util:ensure_binary(Key),
    maps:get(BinKey, Header).


-spec msg_type(#jup_msg{}) -> binary().
msg_type(#jup_msg{} = Msg) ->
    header_entry(Msg, msg_type).

-spec msg_id(#jup_msg{}) -> binary().
msg_id(#jup_msg{} = Msg) ->
    header_entry(Msg, msg_id).


-spec add_headers(#jup_msg{}, #jup_msg{}, atom() | binary()) -> #jup_msg{}.
add_headers(Msg = #jup_msg{}, Parent = #jup_msg{}, MessageType) ->
    Header = #{
      <<"date">> => iso8601:format(os:timestamp()),
      <<"username">> => header_entry(Parent, username),
      <<"session">> => header_entry(Parent, session),
      <<"msg_type">> => jup_util:ensure_binary(MessageType),
      <<"msg_id">> => msg_id(Parent),
      <<"version">> => ?VERSION
     },

    Msg#jup_msg{
      uuids=Parent#jup_msg.uuids,
      header=Header,
      parent_header=Parent#jup_msg.header
     }.
