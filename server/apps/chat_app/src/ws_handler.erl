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
                    {cowboy_websocket, Req0, #state{user_id = UserId, claims = Claims}};
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
    %% ✅ REGISTAR USUÁRIO COMO OFFLINE (user_session + presence_manager)
    user_session:user_offline(UserId),
    presence_manager:user_offline(UserId),
    io:format("🔌 Usuário ~p desconectado e registado como offline~n", [UserId]),
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

%% ✅ HEARTBEAT
handle_websocket_message(#{<<"type">> := <<"heartbeat">>}, #state{user_id = UserId}) ->
    %% ✅ Atualizar heartbeat - usuário está ativo
    presence_manager:user_online(UserId, self()),
    ok;

%% ✅ PRESENCE UPDATE (Manual)
handle_websocket_message(#{<<"type">> := <<"presence_update">>} = Data, #state{user_id = UserId}) ->
    Status = maps:get(<<"status">>, Data, <<"online">>),
    io:format("🔄 Atualização manual de presença para ~p: ~p~n", [UserId, Status]),
    
    case Status of
        <<"online">> ->
            %% Marca como online no presence_manager (broadcast)
            presence_manager:user_online(UserId, self());
        <<"offline">> ->
            %% Marca como offline no presence_manager (broadcast),
            %% MAS MANTÉM a sessão ativa em user_session para receber mensagens!
            presence_manager:user_offline(UserId);
        _ ->
            ok
    end;

%% ✅ MENSAGEM DESCONHECIDA
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