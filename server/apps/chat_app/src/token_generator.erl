-module(token_generator).
-export([generate_token/1]).

generate_token(UserId) ->
    Claims = case UserId of
        "123" ->
            #{
                <<"user_id">> => <<"123">>,
                <<"name">> => <<"Test User 123">>,
                <<"phone">> => <<"+244900000000">>,
                <<"exp">> => 1791975205,
                <<"iat">> => 1760439205
            };
        "456" ->
            #{
                <<"user_id">> => <<"456">>,
                <<"name">> => <<"Test User 456">>,
                <<"phone">> => <<"+244900000001">>,
                <<"exp">> => 1791975205,
                <<"iat">> => 1760439205
            }
    end,
    
    Header = #{<<"alg">> => <<"HS256">>, <<"typ">> => <<"JWT">>},
    
    HeaderBase64 = base64:encode(jsx:encode(Header)),
    ClaimsBase64 = base64:encode(jsx:encode(Claims)),
    
    % Usar a mesma chave secreta do auth_util
    Secret = <<"your-secret-key">>,  % Use a mesma chave do auth_util
    DataToSign = <<HeaderBase64/binary, ".", ClaimsBase64/binary>>,
    Signature = crypto:mac(hmac, sha256, Secret, DataToSign),
    SignatureBase64 = base64:encode(Signature),
    
    Token = <<HeaderBase64/binary, ".", ClaimsBase64/binary, ".", SignatureBase64/binary>>,
    io:format("Token para usuário ~s: ~s~n", [UserId, Token]),
    Token.