%% Debug script para testar revoke_others
-module(debug_revoke).
-export([test/0]).

test() ->
    io:format("🧪 Testando funções de revoke...~n"),
    
    %% Teste 1: JWT creation
    UserMap = #{id => 123, name => <<"Test">>, phone => <<"+1234567890">>},
    case auth_util:create_session_token(UserMap) of
        {ok, Token} ->
            io:format("✅ JWT criado: ~s~n", [Token]),
            
            %% Teste 2: JWT decode
            case auth_util:decode_jwt(Token) of
                {ok, Claims} ->
                    io:format("✅ JWT decodificado: ~p~n", [Claims]),
                    
                    %% Teste 3: Revoke other sessions
                    UserId = maps:get(<<"user_id">>, Claims),
                    SessionId = maps:get(<<"session_id">>, Claims),
                    io:format("🔍 UserId: ~p, SessionId: ~p~n", [UserId, SessionId]),
                    
                    case auth_util:revoke_other_sessions(UserId, SessionId) of
                        ok ->
                            io:format("✅ Revoke funcionou!~n");
                        {error, Reason} ->
                            io:format("❌ Revoke falhou: ~p~n", [Reason])
                    end;
                {error, Reason} ->
                    io:format("❌ JWT decode falhou: ~p~n", [Reason])
            end;
        {error, Reason} ->
            io:format("❌ JWT creation falhou: ~p~n", [Reason])
    end.
