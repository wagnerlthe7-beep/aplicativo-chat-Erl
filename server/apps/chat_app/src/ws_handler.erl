%%%-------------------------------------------------------------------
%%% ws_handler.erl - VERSÃO COMPLETA CORRIGIDA
%%%-------------------------------------------------------------------
-module(ws_handler).

-export([init/2, websocket_init/1, websocket_handle/2, 
         websocket_info/2, terminate/3]).

-record(state, {
    user_id :: binary(),
    claims :: map()
}).

%%%===================================================================
%%% INIT
%%%===================================================================

init(Req0, _State) ->
    %% ✅ DEBUG DETALHADO
    io:format("🎯 WEBSOCKET INIT CHAMADO~n"),
    
    Qs = cowboy_req:parse_qs(Req0),
    io:format("📋 Query Params: ~p~n", [Qs]),
    
    case proplists:get_value(<<"token">>, Qs) of
        undefined ->
            io:format("❌ Token não encontrado nos query params~n"),
            Req1 = cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, "{\"error\":\"token_required\"}", Req0),
            {stop, Req1};
        Token ->
            io:format("🔍 Token encontrado: ~s~n", [Token]),
            
            case auth_util:decode_jwt(Token) of
                {ok, Claims} ->
                    UserId = maps:get(<<"user_id">>, Claims),
                    io:format("✅ ✅ ✅ WebSocket AUTENTICADO para usuário: ~p~n", [UserId]),
                    %% ✅ CONFIGURAR IDLE_TIMEOUT MAIOR PARA EVITAR DESCONEXÕES
                    WsOpts = #{
                        idle_timeout => 300000  %% 5 minutos em milissegundos
                    },
                    {cowboy_websocket, Req0, #state{user_id = UserId, claims = Claims}, WsOpts};
                {error, Reason} ->
                    io:format("❌ ❌ ❌ Token JWT inválido: ~p~n", [Reason]),
                    Req1 = cowboy_req:reply(401, #{
                        <<"content-type">> => <<"application/json">>
                    }, "{\"error\":\"invalid_token\"}", Req0),
                    {stop, Req1}
            end
    end.

%%%===================================================================
%%% WEBSOCKET INIT
%%%===================================================================

websocket_init(State = #state{user_id = UserId}) ->
    io:format("🎯 WebSocket autenticado para usuário: ~p~n", [UserId]),
    
    %% ✅ REGISTAR USUÁRIO COMO ONLINE (user_session + presence_manager)
    user_session:user_online(UserId, self()),
    presence_manager:user_online(UserId, self()),
    io:format("👤 Usuário ~p registado como online~n", [UserId]),
    
    %% ✅ Processar mensagens pendentes (Offline -> Online)
    %% Spawning process to avoid blocking init
    spawn(fun() -> message_router:handle_user_online(UserId) end),

    WelcomeMsg = #{
        <<"type">> => <<"welcome">>,
        <<"user_id">> => UserId,
        <<"message">> => <<"Autenticado e conectado">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    
    {reply, {text, jsx:encode(WelcomeMsg)}, State}.

%%%===================================================================
%%% WEBSOCKET HANDLE
%%%===================================================================

websocket_handle({text, Msg}, State) ->
    io:format("📨 Mensagem recebida: ~p~n", [Msg]),
    
    %% ✅ TENTAR DECODIFICAR DIRETAMENTE PRIMEIRO
    case try_decode_json(Msg) of
        {ok, Data} ->
            io:format("✅ JSON decodificado: ~p~n", [Data]),
            handle_websocket_message(Data, State);
        {error, _Reason} ->
            %% ✅ CORREÇÃO: Limpar caracteres extras
            CleanMsg = clean_message(Msg),
            
            case try_decode_json(CleanMsg) of
                {ok, CleanData} ->
                    io:format("✅ JSON decodificado após limpeza: ~p~n", [CleanData]),
                    handle_websocket_message(CleanData, State);
                {error, _CleanError} ->
                    io:format("❌ JSON inválido mesmo após limpeza: ~p~n", [CleanMsg]),
                    
                    %% ✅ Enviar erro para o cliente
                    ErrorMsg = #{
                        <<"type">> => <<"error">>,
                        <<"error">> => <<"invalid_json_format">>,
                        <<"message">> => <<"Formato JSON inválido">>,
                        <<"timestamp">> => erlang:system_time(second)
                    },
                    self() ! {send_message, ErrorMsg}
            end
    end,
    
    {ok, State};

websocket_handle(_Data, State) ->
    {ok, State}.

%%%===================================================================
%%% WEBSOCKET INFO
%%%===================================================================

websocket_info({send_message, Message}, State) ->
    io:format("📤 Enviando mensagem para cliente: ~p~n", [Message]),
    {reply, {text, jsx:encode(Message)}, State};

websocket_info(_Info, State) ->
    {ok, State}.

%%%===================================================================
%%% TERMINATE
%%%===================================================================

terminate(_Reason, _Req, #state{user_id = UserId}) ->
    %% ✅ REGISTAR USUÁRIO COMO OFFLINE NO USER_SESSION (para indicar que não tem WS ativo)
    %% ⚠️ Importante: ao reconectar, pode existir um WS novo vivo.
    %% Só devemos marcar offline se este PID ainda for o WS atual.
    user_session:user_offline(UserId, self()),
    
    %% ✅ VERIFICAR SE ESTAVA EM BACKGROUND ANTES DE MARCAR OFFLINE NO PRESENCE
    %% Se estava em background, NÃO marcar como offline - manter sessão ativa para FCM
    %% Online/Offline é apenas informativo (UI) - não afeta entrega de mensagens
    Now = erlang:system_time(second),
    
    %% ✅ Verificação robusta: verificar flag de background E último heartbeat
    %% Se heartbeat foi muito recente (< 30s) e socket está fechando, pode indicar background
    %% (heartbeat para quando app vai para background, mas socket pode fechar antes do presence_update)
    WasInBackground = case presence_manager:is_user_in_background(UserId) of
        {ok, true} -> 
            true;
        _ -> 
            %% ✅ Verificação adicional: verificar status atual E último heartbeat
            case presence_manager:get_user_status(UserId) of
                {ok, background, _LastSeen} ->
                    %% Status é background - tratar como background
                    true;
                {ok, online, _LastSeen} ->
                    %% Status é online - verificar se último heartbeat foi muito recente E estava em background
                    %% Se heartbeat foi < 30s E flag estava setado, pode ser que estava em background
                    case presence_manager:get_last_heartbeat(UserId) of
                        {ok, LastHeartbeat, WasBackground} when LastHeartbeat =/= undefined ->
                            HeartbeatAge = Now - LastHeartbeat,
                            %% ✅ Só tratar como background se flag estava setado E heartbeat foi recente
                            %% (indica que estava em background mas socket fechou antes do presence_update ser processado)
                            (WasBackground =:= true) andalso (HeartbeatAge < 60);
                        _ ->
                            false
                    end;
                _ ->
                    %% Offline ou não encontrado - verificar último heartbeat
                    case presence_manager:get_last_heartbeat(UserId) of
                        {ok, LastHeartbeat, WasBackground} when LastHeartbeat =/= undefined ->
                            HeartbeatAge = Now - LastHeartbeat,
                            %% ✅ Só tratar como background se flag estava setado E heartbeat foi recente
                            (WasBackground =:= true) andalso (HeartbeatAge < 60);
                        _ ->
                            false
                    end
            end
    end,
    
    case WasInBackground of
        true ->
            %% ✅ Estava em background - NÃO marcar como offline no presence
            %% MAS: atualizar last_seen para o momento atual (sem broadcast)
            %% Isso mantém o last_seen atualizado sem mudar status visual
            presence_manager:update_last_seen_only(UserId, Now),
            io:format("🌑 Usuário ~p estava em BACKGROUND - mantendo sessão ativa para FCM (last_seen atualizado)~n", [UserId]);
        false ->
            %% Estava em foreground - marcar como offline normalmente
            presence_manager:user_offline(UserId, self()),
            io:format("🔌 Usuário ~p desconectado (foreground) - registado como offline~n", [UserId])
    end,
    ok.

%%%===================================================================
%%% HANDLE MESSAGES - CORRIGIDO PARA SALVAR NA BD PRIMEIRO
%%%===================================================================

%% ✅ MENSAGEM DE TEXTO - FLUXO CORRIGIDO VIA ROUTER
handle_websocket_message(#{<<"type">> := <<"message">>} = Data, #state{user_id = FromId}) ->
    ToId = maps:get(<<"to">>, Data, <<"unknown">>),
    Content = maps:get(<<"content">>, Data, <<"">>),
    
    io:format("MENSAGEM RECEBIDA NO WEBSOCKET~n", []),
    io:format("   De: ~p~n", [FromId]),
    io:format("   Para: ~p~n", [ToId]),
    io:format("   Conteúdo: ~p~n", [Content]),
    
    %% ✅ USAR MESSAGE_ROUTER PARA LÓGICA CENTRALIZADA (SALVAR + ENTREGAR + NOTIFICAR)
    %% Extrair o ID da mensagem do cliente (UUID) se existir
    ClientMsgId = maps:get(<<"message_id">>, Data, undefined),
    
    case message_router:send_message(FromId, ToId, Content, ClientMsgId) of
        {ok, FullMessage, DeliveryStatus} ->
            io:format("   ✅✅✅ MENSAGEM PROCESSADA COM SUCESSO PELO ROUTER. Status: ~p~n", [DeliveryStatus]),
            
            %% ✅ 1. CONFIRMAR AO REMETENTE QUE FOI SALVA (SENT)
            %% Usamos os dados da mensagem salva (incluindo DB ID)
            Confirmation = FullMessage#{
                <<"status">> => <<"sent">>, 
                <<"should_increase_unread">> => false
            },
            self() ! {send_message, Confirmation},
            
            %% ✅ 2. SE FOI ENTREGUE, ENVIAR EVENTO DELIVERED IMEDIATAMENTE
            if DeliveryStatus == delivered ->
                DeliveryMsg = #{
                    <<"type">> => <<"message_delivered">>,
                    <<"message_id">> => maps:get(<<"message_id">>, FullMessage),
                    <<"db_message_id">> => maps:get(<<"db_message_id">>, FullMessage),
                    <<"status">> => <<"delivered">>,
                    <<"delivered_at">> => erlang:system_time(second)
                },
                io:format("   🚀 Enviando evento DELIVERED imediato para remetente~n"),
                self() ! {send_message, DeliveryMsg};
            true ->
                ok
            end;
            
        {error, Error} ->
            io:format("   ❌ ERRO NO ROUTER: ~p~n", [Error]),
            
            ErrorMsg = #{
                <<"type">> => <<"error">>,
                <<"error">> => <<"failed_to_send">>,
                <<"details">> => list_to_binary(io_lib:format("~p", [Error])),
                <<"timestamp">> => erlang:system_time(second)
            },
            self() ! {send_message, ErrorMsg}
    end;

%% ✅ PING
handle_websocket_message(#{<<"type">> := <<"ping">>}, #state{user_id = UserId}) ->
    io:format("🏓 Ping recebido de ~p~n", [UserId]),
    
    %% ✅ Atualizar heartbeat no presence_manager
    presence_manager:user_online(UserId, self()),
    
    %% Responder ao ping
    PongMsg = #{
        <<"type">> => <<"pong">>,
        <<"timestamp">> => erlang:system_time(second)
    },
    self() ! {send_message, PongMsg};

%% ✅ TYPING INDICATOR
handle_websocket_message(#{<<"type">> := <<"typing">>} = Data, #state{user_id = FromId}) ->
    ToId = maps:get(<<"to">>, Data, <<"unknown">>),
    IsTyping = maps:get(<<"is_typing">>, Data, false),
    
    io:format("⌨️  Indicador de digitação de ~p para ~p: ~p~n", [FromId, ToId, IsTyping]),
    
    %% ✅ ENVIAR INDICADOR DE DIGITAÇÃO
    message_router:send_typing_indicator(FromId, ToId, IsTyping);

%% ✅ GET OFFLINE MESSAGES
handle_websocket_message(#{<<"type">> := <<"get_offline_messages">>}, #state{user_id = UserId}) ->
    io:format("📩 Solicitando mensagens offline para ~p~n", [UserId]),
    
    %% ✅ OBTER MENSAGENS OFFLINE
    case message_router:get_offline_messages(UserId) of
        {ok, Messages} ->
            OfflineMsg = #{
                <<"type">> => <<"offline_messages">>,
                <<"messages">> => Messages,
                <<"count">> => length(Messages),
                <<"timestamp">> => erlang:system_time(second)
            },
            self() ! {send_message, OfflineMsg},
            io:format("✅ ~p mensagens offline enviadas para ~p~n", [length(Messages), UserId]);
        {error, Reason} ->
            ErrorMsg = #{
                <<"type">> => <<"error">>,
                <<"error">> => list_to_binary(io_lib:format("~p", [Reason])),
                <<"timestamp">> => erlang:system_time(second)
            },
            self() ! {send_message, ErrorMsg},
            io:format("❌ Erro ao obter mensagens offline: ~p~n", [Reason])
    end;

%% ✅ HEARTBEAT (Foreground - mostra "Online" na UI)
handle_websocket_message(#{<<"type">> := <<"heartbeat">>}, #state{user_id = UserId}) ->
    %% ✅ Atualizar heartbeat - usuário está ativo e visível
    presence_manager:user_online(UserId, self()),
    ok;

%% ✅ HEARTBEAT BACKGROUND (Background - mantém conexão mas esconde "Online")
handle_websocket_message(#{<<"type">> := <<"heartbeat_background">>}, #state{user_id = UserId}) ->
    %% ✅ Atualizar heartbeat MAS não fazer broadcast como "online"
    %% Mantém conexão viva para receber mensagens em tempo real
    presence_manager:user_heartbeat_only(UserId, self()),
    ok;

%% ✅ PRESENCE UPDATE (Manual)
handle_websocket_message(#{<<"type">> := <<"presence_update">>} = Data, #state{user_id = UserId}) ->
    Status = maps:get(<<"status">>, Data, <<"online">>),
    io:format("🔄 Atualização manual de presença para ~p: ~p~n", [UserId, Status]),
    
    case Status of
        <<"online">> ->
            %% Marca como online no presence_manager (broadcast)
            %% Mostra "Online" na UI dos outros users
            presence_manager:user_online(UserId, self());
        <<"background">> ->
            %% App em background - user pode receber FCM mas não está activo
            %% NÃO mostra "Online" na UI, mas mantém sessão para mensagens
            %% Broadcast como "offline" para UI, mas mantém sessão activa
            presence_manager:user_background(UserId, self());
        <<"offline">> ->
            %% Marca como offline no presence_manager (broadcast),
            %% MAS MANTÉM a sessão ativa em user_session para receber mensagens!
            presence_manager:user_offline(UserId);
        _ ->
            ok
    end;

%% ✅ MENSAGENS EDITADAS (NOVO)
handle_websocket_message(#{<<"type">> := <<"message_edited">>} = Data, #state{user_id = UserId}) ->
    _MessageId = maps:get(<<"message_id">>, Data),
    _NewContent = maps:get(<<"content">>, Data),
    
    io:format("✏️ Mensagem editada recebida: ~p~n", [UserId]),
    
    % Atualizar mensagem localmente se necessário
    % (O frontend irá atualizar via WebSocket)
    ok;

%% ✅ MENSAGENS DELETADAS (NOVO)
handle_websocket_message(#{<<"type">> := <<"message_deleted">>} = Data, #state{user_id = UserId}) ->
    io:format("🗑️ Mensagem deletada recebida: ~p -> ~p (deleted_by: ~p)~n", [maps:get(<<"sender_id">>, Data), UserId, maps:get(<<"deleted_by">>, Data)]),
    
    % ✅ ENVIAR PARA O FRONTEND VIA WEBSOCKET
    user_session:send_message(maps:get(<<"sender_id">>, Data), UserId, Data),
    ok;

%% ✅ RESPOSTAS (NOVO)
handle_websocket_message(#{<<"type">> := <<"message_reply">>} = Data, #state{user_id = UserId}) ->
    io:format("💬 Resposta recebida: ~p -> ~p~n", [maps:get(<<"sender_id">>, Data), UserId]),
    
    % Processar resposta
    % (O frontend irá adicionar via WebSocket)
    ok;

%% ✅ MENSAGEM DESCONHECIDA (ESSE DEVE SER O ÚLTIMO!)
handle_websocket_message(Data, _State) ->
    io:format("❓ Mensagem desconhecida: ~p~n", [Data]),
    
    %% Responder com erro de tipo desconhecido
    ErrorMsg = #{
        <<"type">> => <<"error">>,
        <<"error">> => <<"unknown_message_type">>,
        <<"received">> => Data,
        <<"timestamp">> => erlang:system_time(second)
    },
    self() ! {send_message, ErrorMsg}.

%%%===================================================================
%%% FUNÇÕES AUXILIARES
%%%===================================================================

%% ✅ FUNÇÃO AUXILIAR PARA DECODIFICAR JSON
try_decode_json(Msg) ->
    try
        Data = jsx:decode(Msg, [return_maps]),
        {ok, Data}
    catch
        Error:Reason ->
            {error, {Error, Reason}}
    end.

%% ✅ FUNÇÃO PARA LIMPAR MENSAGEM - VERSÃO SIMPLIFICADA E EFICAZ
clean_message(Msg) when is_binary(Msg) ->
    % Remove apenas o prefixo "> " se existir no início
    case Msg of
        <<"> ", Rest/binary>> -> 
            io:format("🔧 Removido prefixo '> '~n"),
            Rest;
        <<"< ", Rest/binary>> -> 
            io:format("🔧 Removido prefixo '< '~n"),
            Rest;
        _ -> 
            io:format("🔧 Nenhum prefixo encontrado~n"),
            Msg
    end;
clean_message(Msg) ->
    io:format("🔧 Mensagem não é binary: ~p~n", [Msg]),
    Msg.