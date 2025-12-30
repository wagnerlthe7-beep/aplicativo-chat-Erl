#!/usr/bin/env escript
%%! -pa _build/default/lib/*/ebin

% Teste para verificar se a solução de delivery com app minimizada funciona
% Este script simula o cenário descrito pelo usuário

-module(test_delivery_background).
-export([main/1]).

main(_) ->
    io:format("🧪 TESTE: Delivery Status com App Minimizada~n"),
    io:format("=" ++ string:chars($=, 50) ++ "~n~n"),
    
    % Iniciar aplicação
    application:ensure_all_started(chat_app),
    
    % Simular dois usuários
    UserId1 = <<"1001">>,
    UserId2 = <<"1002">>,
    
    io:format("📱 Configurando teste para:~n"),
    io:format("   Device A (User ~s): vai minimizar a app~n", [UserId1]),
    io:format("   Device B (User ~s): vai enviar mensagem~n", [UserId2]),
    io:format("~n"),
    
    % 1. Conectar ambos os usuários (simular WebSocket PIDs)
    WsPid1 = spawn(fun() -> receive after 120000 -> ok end end),
    WsPid2 = spawn(fun() -> receive after 120000 -> ok end end),
    
    user_session:user_online(UserId1, WsPid1),
    user_session:user_online(UserId2, WsPid2),
    
    io:format("✅ Ambos usuários conectados~n"),
    
    % 2. Verificar status inicial
    {ok, IsWsAlive1} = user_session:is_websocket_alive(UserId1),
    {ok, IsWsAlive2} = user_session:is_websocket_alive(UserId2),
    
    io:format("🔍 WebSocket Status:~n"),
    io:format("   User ~s: ~p~n", [UserId1, IsWsAlive1]),
    io:format("   User ~s: ~p~n", [UserId2, IsWsAlive2]),
    io:format("~n"),
    
    % 3. Simular app minimizada (sem heartbeat por 60 segundos)
    io:format("📱 Simulando app minimizada para User ~s (sem heartbeat)...~n", [UserId1]),
    timer:sleep(1000), % Simular tempo
    
    % Manter WebSocket ativo mas sem heartbeat atualizado
    presence_manager:user_online(UserId1, WsPid1), % Forçar registro sem heartbeat recente
    
    % 4. Enviar mensagem do Device B para Device A
    io:format("📨 Device B enviando mensagem para Device A...~n"),
    
    case message_router:send_message(UserId2, UserId1, "Teste mensagem com app minimizada") of
        {ok, _Message, delivered} ->
            io:format("✅ SUCESSO: Mensagem marcada como DELIVERED!~n"),
            io:format("   A solução funcionou - WebSocket ativo foi detectado~n");
        {ok, _Message, sent} ->
            io:format("❌ FALHA: Mensagem marcada como SENT apenas~n"),
            io:format("   A solução não funcionou como esperado~n");
        {error, Reason} ->
            io:format("❌ ERRO: ~p~n", [Reason])
    end,
    
    io:format("~n"),
    
    % 5. Verificar status final
    {ok, FinalWsStatus1} = user_session:is_websocket_alive(UserId1),
    io:format("🔍 Status final WebSocket User ~s: ~p~n", [UserId1, FinalWsStatus1]),
    
    % 6. Testar com WebSocket realmente morto
    io:format("~n🧪 Testando com WebSocket realmente morto...~n"),
    exit(WsPid1, kill), % Matar WebSocket
    
    timer:sleep(1000),
    
    case message_router:send_message(UserId2, UserId1, "Teste com WebSocket morto") of
        {ok, _Message, sent} ->
            io:format("✅ CORRETO: Mensagem marcada como SENT (WebSocket morto)~n");
        {ok, _Message, delivered} ->
            io:format("❌ ERRO: Mensagem marcada como DELIVERED com WebSocket morto~n");
        {error, Reason2} ->
            io:format("ℹ️  WebSocket morto detectado: ~p~n", [Reason2])
    end,
    
    io:format("~n" ++ "=" ++ string:chars($=, 50) ++ "~n"),
    io:format("🏁 Teste concluído!~n"),
    
    % Limpar
    user_session:user_offline(UserId1),
    user_session:user_offline(UserId2).
