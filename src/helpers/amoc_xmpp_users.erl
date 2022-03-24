-module(amoc_xmpp_users).

-export([make_jid/1]).
-export([make_jid/2]).
-export([username/1]).
-export([password/1]).

-spec make_jid(amoc_scenario:user_id()) -> binary().
make_jid(Id) ->
    Xmpp_domain =  os:getenv("AMOC_XMPP_DOMAIN"),
    make_jid(Id, <<Xmpp_domain>>).

-spec make_jid(amoc_scenario:user_id(), binary()) -> binary().
make_jid(Id, Host) ->
    ProfileId = username(Id),
    << ProfileId/binary, "@", Host/binary >>.

-spec username(amoc_scenario:user_id()) -> binary().
username(Id) ->
    BinInt = integer_to_binary(Id),
    <<"user_", BinInt/binary>>.

-spec password(amoc_scenario:user_id()) -> binary().
password(Id) ->
    % BinInt = integer_to_binary(Id),
    BinInt = integer_to_binary(Id),
    Claims = [
        {id, BinInt},
        {user, <<"user_", BinInt/binary>>},
        {role, <<"user">>}
    ],
    ExpirationSeconds = 86400,
    JwtSecret = os:getenv("XMPP_JWT_SECRET"),
    Key = <<JwtSecret>>,
    {ok, Token} = jwt:encode(<<"HS256">>, Claims, ExpirationSeconds, Key),
    Token.
