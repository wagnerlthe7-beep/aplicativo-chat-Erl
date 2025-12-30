#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

% Teste rápido para verificar se o delivery funciona com app em background
% Simula o cenário exato: app minimizada mas WebSocket ativo

-module(test_background_delivery).
-export([main/1]).

main(_) ->
    io:format("🧪 TESTE RÁPIDO: Delivery com App em Background~n"),
    io:format("=" ++ string:chars($=, 50) ++ "~n~n"),
    
    % Iniciar aplicação
    application:ensure_all_started(chat_app),
    
    % Simular dois usuários
    UserA = <<"10">>, % Vai minimizar app
    UserB = <<"9">>,  % Vai enviar mensagem
    
    io:format("📱 Cenário:~n"),
    io:format("   User ~s: App minimizada (mas WebSocket ativo)~n", [UserA]),
    io:format("   User ~s: Enviando mensagem~n", [UserB]),
    io:format("~n"),
    
    % 1. Conectar ambos usuários
    WsPidA = spawn(fun() -> receive after 300000 -> ok end end), % 5 min
    WsPidB = spawn(fun() -> receive after 300000 -> ok end end),
    
    user_session:user_online(UserA, WsPidA),
    user_session:user_online(UserB, WsPidB),
    
    io:format("✅ Usuários conectados~n"),
    
    % 2. Verificar se WebSocket está vivo
    io:format("🔍 Verificando WebSocket:~n"),
    io:format("   User ~s: ~p~n", [UserA, user_session:is_websocket_alive(UserA)]),
    io:format("   User ~s: ~p~n", [UserB, user_session:is_websocket_alive(UserB)]),
    io:format("~n"),
    
    % 3. Simular app em background (sem presença offline)
    io:format("📱 Simulando app em background (sem desconectar)~n"),
    % NÃO chamar presence_manager:user_offline(UserA)
    
    % 4. Enviar mensagem
    io:format("📨 Enviando mensagem de ~s para ~s...~n", [UserB, UserA]),
    
    case message_router:send_message(UserB, UserA, "Teste background delivery") of
        {ok, _Msg, delivered} ->
            io:format("✅ SUCESSO! Status: DELIVERED~n"),
            io:format("   WebSocket ativo detectado corretamente~n");
        {ok, _Msg, sent} ->
            io:format("❌ FALHA! Status: SENT (esperado DELIVERED)~n");
        {error, Reason} ->
            io:format("❌ ERRO: ~p~n", [Reason])
    end,
    
    io:format("~n🏁 Teste concluído!~n"),
    
    % Limpar
    user_session:user_offline(UserA),
    user_session:user_offline(UserB).
