-module(jup_msg).

-export([
    decode/2,
    encode/2,
    msg_type/1,
    add_headers/3
]).

-include("internal.hrl").

-define(DELIM, <<"<IDS|MSG>">>).

-type type() :: #jup_msg{}.
-type msg_type() :: atom() | binary().
-type key() :: {crypto:hash_algorithms(), binary()}.

-export_type(
    [
        key/0,
        type/0,
        msg_type/0
    ]
).

-spec decode([binary()], key()) -> type().
decode(MultipartMsg, {SignatureScheme, Key}) ->
    {Uuids, Suffix} = jup_util:split_at_delim(MultipartMsg, ?DELIM),

    [HMAC, Header, ParentHeader, Metadata, Content | ExtraBinaries] = Suffix,

    Ctx0 = crypto:mac_init(hmac, SignatureScheme, Key),
    Ctx1 = crypto:mac_update(Ctx0, Header),
    Ctx2 = crypto:mac_update(Ctx1, ParentHeader),
    Ctx3 = crypto:mac_update(Ctx2, Metadata),
    Ctx4 = crypto:mac_update(Ctx3, Content),

    Signature = jup_util:hexlify(crypto:mac_final(Ctx4)),

    case Signature of
        HMAC1 when HMAC1 =:= HMAC -> ok;
        _ -> error(invalid_signature)
    end,

    Res = #jup_msg{
        uuids = Uuids,
        header = json:decode(Header),
        parent_header = json:decode(ParentHeader),
        metadata = json:decode(Metadata),
        content = json:decode(Content),

        extra_binaries = ExtraBinaries
    },

    Res#jup_msg{type = header_entry(Res, msg_type)}.

-spec encode(type(), key()) -> [binary()].
encode(#jup_msg{} = Msg, {SignatureScheme, Key}) ->
    Ctx0 = crypto:mac_init(hmac, SignatureScheme, Key),
    Header = json_encode(Msg#jup_msg.header),
    Ctx1 = crypto:mac_update(Ctx0, Header),
    ParentHeader = json_encode(Msg#jup_msg.parent_header),
    Ctx2 = crypto:mac_update(Ctx1, ParentHeader),
    Metadata = json_encode(Msg#jup_msg.metadata),
    Ctx3 = crypto:mac_update(Ctx2, Metadata),
    Content = json_encode(Msg#jup_msg.content),
    Ctx4 = crypto:mac_update(Ctx3, Content),

    Signature = jup_util:hexlify(crypto:mac_final(Ctx4)),

    % TODO
    Msg#jup_msg.uuids ++
        [
            ?DELIM,
            Signature,
            Header,
            ParentHeader,
            Metadata,
            Content
            | Msg#jup_msg.extra_binaries
        ].

-spec header_entry(type(), atom()) -> binary().

header_entry(#jup_msg{header = Header}, Key) ->
    BinKey = jup_util:ensure_binary(Key),
    maps:get(BinKey, Header).

-spec msg_type(type()) -> msg_type().
msg_type(#jup_msg{type = Type}) ->
    Type.

-spec add_headers(type(), type() | undefined, msg_type()) -> type().
add_headers(Msg = #jup_msg{}, Parent, MessageType) ->
    MessageType1 = jup_util:ensure_binary(MessageType),
    MsgId = jup_util:get_uuid(),

    {Username, Session} =
        case Parent of
            undefined ->
                {jup_util:get_user(), jup_util:get_uuid()};
            _ ->
                {header_entry(Parent, username), header_entry(Parent, session)}
        end,

    Header = #{
        <<"date">> => iso8601:format(os:timestamp()),
        <<"username">> => Username,
        <<"session">> => Session,
        <<"msg_type">> => MessageType1,
        <<"msg_id">> => MsgId,
        <<"version">> => ?JUP_PROTO_VERSION
    },

    Msg1 =
        Msg#jup_msg{header = Header, type = MessageType1},

    case Parent of
        undefined ->
            Msg1;
        _ ->
            Msg1#jup_msg{
                uuids = Parent#jup_msg.uuids,
                parent_header = Parent#jup_msg.header
            }
    end.

json_encode(Msg) ->
    jup_util:ensure_binary(json:encode(Msg)).
