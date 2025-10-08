%% Teste para executar no shell do Erlang
%% Execute: c(test_jwt). test_jwt:run().

-module(test_jwt).
-export([run/0]).

run() ->
    %% Test JWT creation and decoding
    io:format("🧪 Testing JWT creation and decoding...~n"),
    
    %% Mock user data
    UserMap = #{
        id => 123,
        name => <<"Test User">>,
        phone => <<"+1234567890">>
    },
    
    %% Test JWT creation
    case auth_util:create_session_token(UserMap) of
        {ok, Token} ->
            io:format("✅ JWT created: ~s~n", [Token]),
            
            %% Test JWT decoding
            case auth_util:decode_jwt(Token) of
                {ok, Claims} ->
                    io:format("✅ JWT decoded successfully: ~p~n", [Claims]),
                    UserId = maps:get(<<"user_id">>, Claims),
                    io:format("✅ User ID extracted: ~p~n", [UserId]);
                {error, Reason} ->
                    io:format("❌ JWT decode failed: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("❌ JWT creation failed: ~p~n", [Reason])
    end.
