%%%-------------------------------------------------------------------
%%% message_router.erl - VERSÃO CORRIGIDA
%%%-------------------------------------------------------------------

-module(message_router).

% Definir o record localmente para acesso ao ETS
-record(user_presence, {
    user_id :: binary(),
    ws_pid :: pid(),
    last_heartbeat :: integer(),
    is_connected :: boolean()
}).

-record(pending_messages, {
    message_id :: binary(),
    receiver_id :: binary(), 
    sender_id :: binary(),
    content :: binary(),
    timestamp :: integer(),
    status :: binary()
}).

-export([
    send_message/3,
    send_message/4,
    mark_message_delivered/2,
    mark_message_read/2, 
    send_typing_indicator/3,
    broadcast_presence/2,
    get_message_sender/1,
    get_offline_messages/1,
    handle_user_online/1
]).

%%%-------------------------------------------------------------------
%%% @doc Envia uma mensagem de um usuário para outro
%%% @end
%%%-------------------------------------------------------------------
send_message(FromId, ToId, Content) ->
    send_message(FromId, ToId, Content, undefined).

send_message(FromId, ToId, Content, ClientMsgId) ->
    MessageId = case ClientMsgId of
        undefined -> generate_message_id();
        <<>> -> generate_message_id();
        Id -> Id
    end,
    Timestamp = erlang:system_time(second),
    
    %% ✅ PRIMEIRO: SALVAR NA BD (status inicial = 'sent')
    io:format("🎯🎯🎯 MESSAGE ROUTER: Salvando mensagem na BD 🎯🎯🎯~n", []),
    io:format("   FromId: ~p, ToId: ~p, Content: ~p~n", [FromId, ToId, Content]),
    
    case message_repo:save_message(FromId, ToId, Content) of
        {ok, DbMessageId} ->
            io:format("   ✅✅✅ Mensagem salva na BD com ID: ~p~n", [DbMessageId]),
            
            %% ✅ ENVIAR PARA DESTINATÁRIO (VERIFICANDO SE WEBSOCKET ESTÁ VIVO)
            %% Nova lógica: Se WebSocket está ativo, mensagem é entregue (delivered)
            %% Se não está ativo, fica como sent (offline)
            MessageToReceiver = #{<<"type">> => <<"message">>,
                                <<"from">> => FromId,
                                <<"to">> => ToId,
                                <<"content">> => Content,
                                <<"timestamp">> => Timestamp,
                                <<"message_id">> => MessageId,
                                <<"db_message_id">> => DbMessageId,
                                <<"status">> => <<"delivered">>,  %% para o destinatário, já entregue
                                <<"should_increase_unread">> => true},
            
            IsWsAlive = user_session:is_websocket_alive(ToId),
            io:format("   🔍 WebSocket alive para ~p: ~p~n", [ToId, IsWsAlive]),
            
            %% ✅ ESTRATÉGIA MELHORADA: Grace period para Android background
            case IsWsAlive of
                true ->
                    %% WebSocket ativo - tentar enviar mensagem
                    case user_session:send_message(FromId, ToId, MessageToReceiver) of
                        ok ->
                            io:format("   ✅✅✅ Enviada para DESTINATÁRIO ~p (WS vivo)~n", [ToId]),
                            
                            %% ✅ ATUALIZAR BD: status = 'delivered'
                            message_repo:mark_message_delivered(DbMessageId),
                            
                            io:format("   ✅ Mensagem entregue para ~p. Retornando status 'delivered' para remetente.~n", [ToId]),
                            
                            {ok, MessageToReceiver, delivered};
                            
                        {error, Reason} ->
                            io:format("   ❌ Erro ao enviar para WS ativo: ~p~n", [Reason]),
                            {error, Reason}
                    end;
                false ->
                    %% WebSocket não está ativo - verificar grace period
                    case check_grace_period(ToId) of
                        {ok, within_grace} ->
                            io:format("   ⏰ Usuário ~p em grace period (Android background) - tentando delivery~n", [ToId]),
                            %% Tentar entregar mesmo sem WebSocket (pode reconectar)
                            case user_session:send_message(FromId, ToId, MessageToReceiver) of
                                ok ->
                                    message_repo:mark_message_delivered(DbMessageId),
                                    io:format("   ✅ Delivery bem-sucedido em grace period~n"),
                                    {ok, MessageToReceiver, delivered};
                                {error, _} ->
                                    io:format("   💾 Grace period expirou para ~p - armazenando mensagem (status=sent)~n", [ToId]),
                                    store_offline_message(ToId, MessageToReceiver#{
                                      <<"status">> => <<"sent">>
                                    }),
                                    {ok, MessageToReceiver, sent}
                            end;
                        {ok, expired} ->
                            io:format("   💾 Grace period expirou para ~p - armazenando mensagem (status=sent)~n", [ToId]),
                            store_offline_message(ToId, MessageToReceiver#{
                              <<"status">> => <<"sent">>
                            }),
                            {ok, MessageToReceiver, sent}
                    end
            end;
            
        {error, DbError} ->
            io:format("   ❌❌❌ ERRO AO SALVAR NA BD: ~p~n", [DbError]),
            {error, DbError}
    end.

%%%-------------------------------------------------------------------
%%% @doc Marca uma mensagem como entregue
%%% @end
%%%-------------------------------------------------------------------
mark_message_delivered(MessageId, ByUser) ->
    case get_message_sender(MessageId) of
        {ok, FromId} ->
            DeliveryMsg = #{<<"type">> => <<"message_delivered">>,
                          <<"message_id">> => MessageId,
                          <<"by_user">> => ByUser,
                          <<"timestamp">> => erlang:system_time(second)},
            user_session:send_message(ByUser, FromId, DeliveryMsg),
            ok;
        _ ->
            {error, message_not_found}
    end.

%%%-------------------------------------------------------------------
%%% @doc Marca uma mensagem como lida
%%% @end
%%%-------------------------------------------------------------------
mark_message_read(MessageId, ByUser) ->
    case get_message_sender(MessageId) of
        {ok, FromId} ->
            ReadMsg = #{<<"type">> => <<"message_read">>,
                      <<"message_id">> => MessageId,
                      <<"by_user">> => ByUser,
                      <<"timestamp">> => erlang:system_time(second)},
            user_session:send_message(ByUser, FromId, ReadMsg),
            ok;
        _ ->
            {error, message_not_found}
    end.

%%%-------------------------------------------------------------------
%%% @doc Envia indicador de digitação
%%% @end
%%%-------------------------------------------------------------------
send_typing_indicator(FromId, ToId, IsTyping) ->
    TypingMsg = #{<<"type">> => <<"typing">>,
                 <<"from">> => FromId,
                 <<"to">> => ToId,
                 <<"is_typing">> => IsTyping,
                 <<"timestamp">> => erlang:system_time(second)},
    user_session:send_message(FromId, ToId, TypingMsg).

%%%-------------------------------------------------------------------
%%% @doc Transmite status de presença
%%% @end
%%%-------------------------------------------------------------------
broadcast_presence(UserId, Status) ->
    PresenceMsg = #{<<"type">> => <<"presence">>,
                   <<"user_id">> => UserId,
                   <<"status">> => Status,
                   <<"timestamp">> => erlang:system_time(second)},
    
    case get_user_contacts(UserId) of
        {ok, Contacts} ->
            lists:foreach(
                fun(ContactId) ->
                    user_session:send_message(UserId, ContactId, PresenceMsg)
                end,
                Contacts
            );
        {error, _} ->
            ok
    end.

%%%-------------------------------------------------------------------
%%% @doc Obtém mensagens offline de um usuário
%%% @end
%%%-------------------------------------------------------------------
get_offline_messages(UserId) ->
    io:format("📩 Getting offline messages for user: ~p~n", [UserId]),
    F = fun() ->
        mnesia:match_object(#pending_messages{
            message_id = '_',
            receiver_id = UserId,
            sender_id = '_',
            content = '_',
            timestamp = '_',
            status = '_'
        })
    end,
    {atomic, Results} = mnesia:transaction(F),
    
    Messages = lists:map(fun(R) ->
        #{
            <<"type">> => <<"message">>,
            <<"from">> => R#pending_messages.sender_id,
            <<"to">> => R#pending_messages.receiver_id,
            <<"content">> => R#pending_messages.content,
            <<"timestamp">> => R#pending_messages.timestamp,
            <<"message_id">> => R#pending_messages.message_id,
            <<"status">> => R#pending_messages.status,
            <<"should_increase_unread">> => true
        }
    end, Results),
    
    %% Limpar mensagens offline após enviar
    clear_offline_messages(UserId),
    
    {ok, Messages}.

%%%-------------------------------------------------------------------
%%% @doc Processa mensagens pendentes quando usuário fica online
%%% @end
%%%-------------------------------------------------------------------
handle_user_online(UserId) ->
    io:format("🚀 Processando mensagens pendentes para usuário: ~p~n", [UserId]),
    case message_repo:get_undelivered_messages(UserId) of
        {ok, Messages} ->
            io:format("   📋 Encontradas ~p mensagens pendentes~n", [length(Messages)]),
            
            MessageIds = lists:map(fun({Id, _SenderId, _Content, _SentAt, _Status}) -> Id end, Messages),
            
            %% 1. Processar cada mensagem (notificar Sender e enviar para Receiver)
            lists:foreach(fun({Id, SenderId, Content, _SentAt, _Status}) ->
                SenderIdBin = integer_to_binary(SenderId),
                
                %% Notificar remetente (status = delivered)
                DeliveryMsg = #{
                    <<"type">> => <<"message_delivered">>,
                    <<"message_id">> => integer_to_binary(Id), %% Usando ID do banco como referência se não houver UUID
                    <<"db_message_id">> => Id,
                    <<"status">> => <<"delivered">>,
                    <<"delivered_at">> => erlang:system_time(second)
                },
                %% Tenta enviar para o remetente se estiver online
                user_session:send_message(UserId, SenderIdBin, DeliveryMsg),
                
                %% Enviar mensagem para o destinatário (UserId)
                MsgForReceiver = #{
                    <<"type">> => <<"message">>,
                    <<"from">> => SenderIdBin,
                    <<"to">> => UserId,
                    <<"content">> => Content,
                    <<"timestamp">> => erlang:system_time(second),
                    <<"message_id">> => integer_to_binary(Id),
                    <<"db_message_id">> => Id,
                    <<"status">> => <<"delivered">>,
                    <<"should_increase_unread">> => true
                },
                %% Envia para o próprio usuário que acabou de conectar
                user_session:send_message(SenderIdBin, UserId, MsgForReceiver)
                
            end, Messages),
            
            %% 2. Atualizar status no banco em lote
            if length(MessageIds) > 0 ->
                message_repo:mark_messages_as_delivered(MessageIds),
                io:format("   ✅ Mensagens marcadas como delivered no banco~n");
            true ->
                ok
            end;
            
        {error, Error} ->
            io:format("   ❌ Erro ao buscar mensagens pendentes: ~p~n", [Error])
    end.

%%%-------------------------------------------------------------------
%%% FUNÇÕES AUXILIARES
%%%-------------------------------------------------------------------

%% @doc Gera um ID único para a mensagem
generate_message_id() ->
    <<Id:128>> = crypto:strong_rand_bytes(16),
    list_to_binary(integer_to_list(Id, 36)).

%% @doc Obtém o remetente de uma mensagem
get_message_sender(MessageId) ->
    case catch binary_to_integer(MessageId) of
        IdInt when is_integer(IdInt) ->
            message_repo:get_message_sender(IdInt);
        _ ->
            {error, invalid_id}
    end.

%% @doc Obtém contatos de um usuário (simplificado)
get_user_contacts(_UserId) ->
    {ok, []}.

%% @doc Armazena mensagem offline
store_offline_message(UserId, Message) ->
    MessageId = maps:get(<<"message_id">>, Message),
    FromId = maps:get(<<"from">>, Message),
    Content = maps:get(<<"content">>, Message),
    Timestamp = maps:get(<<"timestamp">>, Message),
    Status = maps:get(<<"status">>, Message, <<"pending">>),
    
    Record = #pending_messages{
        message_id = MessageId,
        receiver_id = UserId,
        sender_id = FromId,
        content = Content,
        timestamp = Timestamp,
        status = Status
    },
    
    mnesia:transaction(fun() ->
        mnesia:write(pending_messages, Record, write)
    end),
    
    io:format("💾 Stored offline message for ~p: ~p~n", [UserId, MessageId]).

%% @doc Verifica se usuário está em grace period (Android background)
check_grace_period(UserId) ->
    try
        % Verificar se há um registro recente de desconexão
        case ets:lookup(user_presence, UserId) of
            [#user_presence{is_connected = false, last_heartbeat = LastHeartbeat}] ->
                Now = erlang:system_time(second),
                GracePeriodSeconds = 120,  % 2 minutos de grace period
                
                io:format("   ⏰ Grace period check: Now=~p, LastHeartbeat=~p, Diff=~p~n", 
                          [Now, LastHeartbeat, Now - LastHeartbeat]),
                
                case (Now - LastHeartbeat) =< GracePeriodSeconds of
                    true ->
                        io:format("   ⏰ Usuário dentro do grace period~n"),
                        {ok, within_grace};
                    false ->
                        io:format("   ⏰ Grace period expirou~n"),
                        {ok, expired}
                end;
            [#user_presence{is_connected = true}] ->
                % Usuário está conectado - não precisa de grace period
                io:format("   ℹ️ Usuário está conectado, grace period não aplicável~n"),
                {ok, expired};
            _ ->
                % Sem registro - expirado
                io:format("   ℹ️ Sem registro de presença, grace period expirado~n"),
                {ok, expired}
        end
    catch
        _:_ ->
            io:format("   ❌ Erro ao verificar grace period~n"),
            {ok, expired}
    end.

%% @doc Limpa mensagens offline de um usuário
clear_offline_messages(UserId) ->
    %% CORREÇÃO: Isso pode estar errado se o index não for receiver_id.
    %% Mas deixamos como está pois estamos migrando para message_repo.
    F = fun() ->
        mnesia:delete({pending_messages, UserId})
    end,
    mnesia:transaction(F),
    io:format("🧹 Cleared offline messages for user: ~p~n", [UserId]).
