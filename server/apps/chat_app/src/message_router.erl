%%%-------------------------------------------------------------------
%%% message_router.erl - VERSÃO CORRIGIDA
%%%-------------------------------------------------------------------

-module(message_router).

-record(pending_messages, {
    message_id :: binary(),
    receiver_id :: binary(), 
    sender_id :: binary(),
    %% Armazenamos o MAP COMPLETO da mensagem para preservar metadados (ex: reply_to_*).
    %% Para mensagens antigas, pode ser apenas o conteúdo (binary).
    content :: term(),
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
    handle_user_online/1,
    store_offline_message/2,
    % Funções para notificações (que não estão sendo usadas ainda, mas serão)
    notify_message_edited/4,
    notify_message_deleted/4,
    notify_message_reply/5,
    
    % Funções auxiliares
    get_message_details/1,
    get_message_replies/1
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
            
            %% ✅ ESTRATÉGIA ESTILO WHATSAPP: 
            %% 1. Sempre tentar WebSocket primeiro (se estiver conectado)
            %% 2. Se WebSocket não estiver disponível, usar FCM
            %% 3. Online/Offline é apenas informativo (UI) - não afeta entrega de mensagens
            %% 4. ACK é o que importa para marcar como delivered
            IsWsAlive = user_session:is_websocket_alive(ToId),
            io:format("   🔍 WebSocket alive para ~p: ~p~n", [ToId, IsWsAlive]),
            
            %% ✅ TENTAR ENTREGAR VIA WEBSOCKET SE ESTIVER DISPONÍVEL
            %% Não importa se está "online" ou "offline" no presence - isso é só UI
            %% O que importa é se o WebSocket está vivo para entregar em tempo real
            case IsWsAlive of
                true ->
                    %% ✅ WebSocket está vivo - tentar entregar via WebSocket (tempo real)
                    case user_session:send_message(FromId, ToId, MessageToReceiver) of
                        ok ->
                            io:format("   ✅✅✅ Enviada para DESTINATÁRIO ~p via WebSocket~n", [ToId]),
                            
                            %% ✅ NOTA: NÃO marcamos como delivered aqui!
                            %% O status só muda para 'delivered' quando recebermos o ACK do cliente
                            %% Isso é o comportamento correto estilo WhatsApp
                            
                            %% ✅ ENVIAR ATUALIZAÇÃO PARA CHAT LIST PAGE
                            send_chat_list_update(FromId, ToId, Content, DbMessageId),
                            
                            io:format("   ✅ Mensagem enviada via WS para ~p. Aguardando ACK para marcar como delivered.~n", [ToId]),
                            
                            %% Retornamos 'sent' porque ainda não temos o ACK
                            %% O cliente vai enviar o ACK e aí sim marcamos como delivered
                            {ok, MessageToReceiver, sent};
                            
                        {error, Reason} ->
                            io:format("   ❌ Erro ao enviar via WS: ~p - usando FCM como fallback~n", [Reason]),
                            %% ✅ Se falhar ao enviar via WebSocket, usar FCM
                            store_offline_message(ToId, MessageToReceiver#{
                              <<"status">> => <<"sent">>
                            }),
                            
                            %% ✅ ENVIAR PUSH NOTIFICATION VIA FCM (fallback)
                            spawn(fun() -> 
                                send_fcm_notification(ToId, FromId, DbMessageId, Content)
                            end),
                            
                            {ok, MessageToReceiver, sent}
                    end;
                false ->
                    %% ✅ WebSocket não está disponível - usar FCM (estilo WhatsApp)
                    %% Isso acontece quando app está fechada ou em background
                    io:format("   📱 WebSocket não disponível para ~p - usando FCM~n", [ToId]),
                    store_offline_message(ToId, MessageToReceiver#{
                      <<"status">> => <<"sent">>
                    }),
                    
                    %% ✅ ENVIAR PUSH NOTIFICATION VIA FCM
                    %% Esta é a parte CRÍTICA para o sistema estilo WhatsApp
                    %% A push notification vai acordar a app do usuário mesmo fechada
                    %% A app então envia o ACK e a mensagem é marcada como delivered
                    spawn(fun() -> 
                        send_fcm_notification(ToId, FromId, DbMessageId, Content)
                    end),
                    
                    {ok, MessageToReceiver, sent}
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
    case get_message_details(MessageId) of
        {ok, MessageDetails} ->
            FromId = integer_to_binary(maps:get(sender_id, MessageDetails)),
            IsEdited = maps:get(is_edited, MessageDetails, false),
            DeliveryMsg = #{<<"type">> => <<"message_delivered">>,
                          <<"message_id">> => MessageId,
                          <<"by_user">> => ByUser,
                          <<"is_edited">> => IsEdited,
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
    case get_message_details(MessageId) of
        {ok, MessageDetails} ->
            FromId = integer_to_binary(maps:get(sender_id, MessageDetails)),
            IsEdited = maps:get(is_edited, MessageDetails, false),
            ReadMsg = #{<<"type">> => <<"message_read">>,
                      <<"message_id">> => MessageId,
                      <<"by_user">> => ByUser,
                      <<"is_edited">> => IsEdited,
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
    %% ✅ Typing não é crítico - se usuário estiver offline, apenas ignorar silenciosamente
    %% Não retornar erro porque typing é apenas informativo (estilo WhatsApp)
    case user_session:send_message(FromId, ToId, TypingMsg) of
        ok -> ok;
        {error, user_offline} -> 
            %% Usuário offline (app fechada/background) - typing não é crítico, apenas ignorar
            io:format("⌨️  Typing ignorado para ~p (offline/background)~n", [ToId]),
            ok;
        {error, _Reason} -> 
            %% Outro erro - também ignorar (typing não é crítico)
            io:format("⌨️  Typing ignorado para ~p (erro: ~p)~n", [ToId, _Reason]),
            ok
    end.

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
        ContentData = R#pending_messages.content,
        % Se content for um mapa completo, extrai o texto e metadados de reply.
        {ContentText, ReplyFields} = case is_map(ContentData) of
            true ->
                {
                    maps:get(<<"content">>, ContentData, ContentData),
                    maps:with([<<"reply_to_id">>, <<"reply_to_text">>, <<"reply_to_sender_name">>, <<"reply_to_sender_id">>], ContentData)
                };
            false ->
                {ContentData, #{}}
        end,
        maps:merge(#{
            <<"type">> => <<"message">>,
            <<"from">> => R#pending_messages.sender_id,
            <<"to">> => R#pending_messages.receiver_id,
            <<"content">> => ContentText,
            <<"timestamp">> => R#pending_messages.timestamp,
            <<"message_id">> => R#pending_messages.message_id,
            <<"status">> => R#pending_messages.status,
            <<"should_increase_unread">> => true
        }, ReplyFields)
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
            
            MessageIds = lists:map(fun(Msg) -> maps:get(<<"id">>, Msg) end, Messages),
            
            %% 1. Processar cada mensagem (verificar se não foi deletada)
            lists:foreach(fun(Msg) ->
                Id = maps:get(<<"id">>, Msg),
                SenderId = maps:get(<<"sender_id">>, Msg),
                Content = maps:get(<<"content">>, Msg),
                SentAt = erlang:system_time(second),
                ReplyToId = maps:get(<<"reply_to_id">>, Msg, null),
                ReplyToText = maps:get(<<"reply_to_text">>, Msg, null),
                ReplyToSenderId = maps:get(<<"reply_to_sender_id">>, Msg, null),
                ReplyToSenderName = maps:get(<<"reply_to_sender_name">>, Msg, null),
                SenderIdBin = integer_to_binary(SenderId),
                
                %% ✅ VERIFICAR SE MENSAGEM FOI DELETADA ANTES DE ENTREGAR
                case message_repo:is_message_deleted(Id) of
                    {ok, false} ->
                        %% MENSAGEM NÃO DELETADA - ENTREGAR NORMALMENTE
                        io:format("   ✅ Entregando mensagem ~p para usuário ~p~n", [Id, UserId]),
                        
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
                            <<"timestamp">> => SentAt,
                            <<"message_id">> => integer_to_binary(Id),
                            <<"db_message_id">> => Id,
                            <<"status">> => <<"delivered">>,
                            <<"should_increase_unread">> => true,
                            %% Metadados de reply
                            <<"reply_to_id">> => ReplyToId,
                            <<"reply_to_text">> => ReplyToText,
                            <<"reply_to_sender_id">> => ReplyToSenderId,
                            <<"reply_to_sender_name">> => ReplyToSenderName
                        },
                        %% Envia para o próprio usuário que acabou de conectar
                        user_session:send_message(SenderIdBin, UserId, MsgForReceiver);
                    
                    {ok, true} ->
                        %% MENSAGEM DELETADA - ENTREGAR COMO MENSAGEM DELETADA PERSONALIZADA
                        io:format("   🗑️ Entregando mensagem deletada ~p para usuário ~p~n", [Id, UserId]),
                        
                        %% Buscar quem deletou a mensagem
                        case message_repo:get_message_deleter(Id) of
                            {ok, DeletedBy} ->
                                %% Personalizar mensagem baseado em quem deletou (perspectiva do destinatário)
                                DeletedContent = case DeletedBy of
                                    UserId -> unicode:characters_to_binary("⊗ Eliminou esta mensagem");  % Você apagou
                                    _ -> unicode:characters_to_binary("⊗ Esta mensagem foi apagada")  % Outro apagou
                                end,
                                
                                io:format("   🔍 DEBUG DeletedContent: ~p~n", [DeletedContent]),
                                
                                %% Notificar remetente (status = delivered)
                                DeliveryMsg = #{
                                    <<"type">> => <<"message_delivered">>,
                                    <<"message_id">> => integer_to_binary(Id),
                                    <<"db_message_id">> => Id,
                                    <<"status">> => <<"delivered">>,
                                    <<"delivered_at">> => erlang:system_time(second)
                                },
                                user_session:send_message(UserId, SenderIdBin, DeliveryMsg),
                                
                                %% Enviar mensagem personalizada para o destinatário
                                DeletedMsgForReceiver = #{
                                    <<"type">> => <<"message">>,
                                    <<"from">> => SenderIdBin,
                                    <<"to">> => UserId,
                                    <<"content">> => DeletedContent,
                                    <<"timestamp">> => SentAt,
                                    <<"message_id">> => integer_to_binary(Id),
                                    <<"db_message_id">> => Id,
                                    <<"status">> => <<"delivered">>,
                                    <<"should_increase_unread">> => false,  % Não aumentar unread para mensagem deletada
                                    <<"deleted_by">> => integer_to_binary(DeletedBy),
                                    <<"reply_to_id">> => ReplyToId,
                                    <<"reply_to_text">> => ReplyToText,
                                    <<"reply_to_sender_id">> => ReplyToSenderId,
                                    <<"reply_to_sender_name">> => ReplyToSenderName
                                },
                                user_session:send_message(SenderIdBin, UserId, DeletedMsgForReceiver);
                            
                            {error, _} ->
                                %% Se não conseguir quem deletou, usar mensagem genérica
                                GenericDeletedMsg = #{
                                    <<"type">> => <<"message">>,
                                    <<"from">> => SenderIdBin,
                                    <<"to">> => UserId,
                                    <<"content">> => unicode:characters_to_binary("⊗ Esta mensagem foi apagada"),
                                    <<"timestamp">> => SentAt,
                                    <<"message_id">> => integer_to_binary(Id),
                                    <<"db_message_id">> => Id,
                                    <<"status">> => <<"delivered">>,
                                    <<"should_increase_unread">> => false
                                },
                                user_session:send_message(SenderIdBin, UserId, GenericDeletedMsg)
                        end;
                    
                    {error, Reason} ->
                        %% ERRO AO VERIFICAR - LOG E PULAR
                        io:format("   ❌ Erro ao verificar status da mensagem ~p: ~p~n", [Id, Reason])
                end
                
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
    %% Guardar o MAP COMPLETO para preservar metadados (ex: reply_to_*).
    Content = Message,
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

%% @doc Limpa mensagens offline de um usuário
clear_offline_messages(UserId) ->
    %% CORREÇÃO: Isso pode estar errado se o index não for receiver_id.
    %% Mas deixamos como está pois estamos migrando para message_repo.
    F = fun() ->
        mnesia:delete({pending_messages, UserId})
    end,
    mnesia:transaction(F),
    io:format("🧹 Cleared offline messages for user: ~p~n", [UserId]).


%%%===================================================================
%%% Funções para operações avançadas de mensagens
%%%===================================================================

%% @doc Notificar edição de mensagem
notify_message_edited(MessageId, UpdatedContent, SenderId, ReceiverId) ->
    try
        Notification = #{
            <<"type">> => <<"message_edited">>,
            <<"message_id">> => MessageId,
            <<"sender_id">> => SenderId,
            <<"receiver_id">> => ReceiverId,
            <<"content">> => UpdatedContent,
            <<"timestamp">> => erlang:system_time(second)
        },
        
        user_session:send_message(SenderId, ReceiverId, Notification),
        io:format("📡 Notificação de edição enviada: ~p -> ~p~n", [SenderId, ReceiverId]),
        ok
    catch
        Error:Reason ->
            io:format("❌ Erro ao notificar edição: ~p:~p~n", [Error, Reason]),
            {error, Reason}
    end.

%% @doc Notificar deleção de mensagem
notify_message_deleted(MessageId, SenderId, ReceiverId, Reason) ->
    try
        % Descobrir quem deletou (será o usuário atual da sessão)
        DeletedBy = case Reason of
            <<"user_delete">> -> SenderId;  % Remetente deletou
            _ -> SenderId  % Por padrão, assume que foi o remetente
        end,
        
        Notification = #{
            <<"type">> => <<"message_deleted">>,
            <<"message_id">> => MessageId,
            <<"sender_id">> => SenderId,
            <<"receiver_id">> => ReceiverId,
            <<"deleted_by">> => DeletedBy,
            <<"reason">> => Reason,
            <<"timestamp">> => erlang:system_time(second)
        },
        
        % Enviar para o destinatário
        user_session:send_message(SenderId, ReceiverId, Notification),
        % Enviar para o próprio remetente (para atualizar o chat dele também)
        user_session:send_message(SenderId, SenderId, Notification),
        io:format("📡 Notificação de deleção enviada: ~p -> ~p e ~p -> ~p (deleted_by: ~p)~n", [SenderId, ReceiverId, SenderId, SenderId, DeletedBy]),
        ok
    catch
        Error:Reason ->
            io:format("❌ Erro ao notificar deleção: ~p:~p~n", [Error, Reason]),
            {error, Reason}
    end.

%% @doc Notificar resposta a mensagem
notify_message_reply(ReplyMessageId, OriginalMessageId, SenderId, ReceiverId, Content) ->
    try
        Notification = #{
            <<"type">> => <<"message_reply">>,
            <<"message_id">> => ReplyMessageId,
            <<"original_message_id">> => OriginalMessageId,
            <<"sender_id">> => SenderId,
            <<"receiver_id">> => ReceiverId,
            <<"content">> => Content,
            <<"timestamp">> => erlang:system_time(second)
        },
        
        user_session:send_message(SenderId, ReceiverId, Notification),
        io:format("📡 Notificação de resposta enviada: ~p -> ~p~n", [SenderId, ReceiverId]),
        ok
    catch
        Error:Reason ->
            io:format("❌ Erro ao notificar resposta: ~p:~p~n", [Error, Reason]),
            {error, Reason}
    end.

%% @doc Obter informações completas da mensagem
get_message_details(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            MsgIdInt = binary_to_integer_wrapper(MessageId),
            
            Sql = "SELECT m.id, m.sender_id, m.receiver_id, m.content, 
                          m.sent_at, m.status, m.is_edited, m.edit_count,
                          m.reply_to_id, m.is_deleted, m.deleted_by,
                          m.delete_reason, m.deleted_at,
                          u1.name as sender_name, u2.name as receiver_name
                   FROM messages m
                   LEFT JOIN users u1 ON m.sender_id = u1.id
                   LEFT JOIN users u2 ON m.receiver_id = u2.id
                   WHERE m.id = $1",
            
            case epgsql:equery(Conn, Sql, [MsgIdInt]) of
                {ok, _, [Row]} ->
                    {Id, SenderId, ReceiverId, Content, SentAt, Status, 
                     IsEdited, EditCount, ReplyToId, IsDeleted, DeletedBy,
                     DeleteReason, DeletedAt, SenderName, ReceiverName} = Row,
                    
                    MessageDetails = #{
                        id => Id,
                        sender_id => SenderId,
                        receiver_id => ReceiverId,
                        content => Content,
                        sent_at => SentAt,
                        status => Status,
                        is_edited => IsEdited,
                        edit_count => EditCount,
                        reply_to_id => ReplyToId,
                        is_deleted => IsDeleted,
                        deleted_by => DeletedBy,
                        delete_reason => DeleteReason,
                        deleted_at => DeletedAt,
                        sender_name => SenderName,
                        receiver_name => ReceiverName
                    },
                    {ok, MessageDetails};
                {ok, _, []} ->
                    {error, not_found};
                {error, Error} ->
                    {error, Error}
            end
        catch
            _:_ -> {error, invalid_id}
        end
    end).

%% @doc Buscar mensagens respondidas
get_message_replies(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            MsgIdInt = binary_to_integer_wrapper(MessageId),
            
            Sql = "SELECT m.id, m.sender_id, m.receiver_id, m.content, 
                          m.sent_at, m.status, u.name as sender_name
                   FROM messages m
                   JOIN users u ON m.sender_id = u.id
                   WHERE m.reply_to_id = $1
                   ORDER BY m.sent_at ASC",
            
            case epgsql:equery(Conn, Sql, [MsgIdInt]) of
                {ok, _, Rows} ->
                    Replies = lists:map(fun(Row) ->
                        {Id, SenderId, ReceiverId, Content, SentAt, Status, SenderName} = Row,
                        #{
                            id => Id,
                            sender_id => SenderId,
                            receiver_id => ReceiverId,
                            content => Content,
                            sent_at => SentAt,
                            status => Status,
                            sender_name => SenderName
                        }
                    end, Rows),
                    {ok, Replies};
                {error, Error} ->
                    {error, Error}
            end
        catch
            _:_ -> {error, invalid_id}
        end
    end).

%% Helper para conversão
binary_to_integer_wrapper(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary));
binary_to_integer_wrapper(Integer) when is_integer(Integer) ->
    Integer.

%% @doc Enviar atualização para chat list page
send_chat_list_update(FromId, ToId, Content, MessageId) ->
    try
        %% Buscar telefone do remetente para facilitar resolução na agenda do cliente
        FromPhone = case user_info_handler:get_user_from_db(FromId) of
            {ok, UserInfo} -> maps:get(<<"phone">>, UserInfo, <<"">>);
            _ -> <<"">>
        end,

        %% Notificação para o remetente (atualizar chat list dele)
        SenderUpdate = #{
            <<"type">> => <<"chat_list_update">>,
            <<"message_id">> => integer_to_binary(MessageId),
            <<"from">> => FromId,
            <<"to">> => ToId,
            <<"content">> => Content,
            <<"phone">> => FromPhone, %% Incluir telefone
            <<"timestamp">> => erlang:system_time(second),
            <<"action">> => <<"new_message">>
        },
        user_session:send_message(ToId, FromId, SenderUpdate),
        
        %% Notificação para o destinatário (atualizar chat list dele)
        ReceiverUpdate = #{
            <<"type">> => <<"chat_list_update">>,
            <<"message_id">> => integer_to_binary(MessageId),
            <<"from">> => FromId,
            <<"to">> => ToId,
            <<"content">> => Content,
            <<"phone">> => FromPhone, %% Incluir telefone
            <<"timestamp">> => erlang:system_time(second),
            <<"action">> => <<"new_message">>
        },
        user_session:send_message(FromId, ToId, ReceiverUpdate),
        
        io:format("   📋 Chat list update sent for message ~p (Phone: ~s)~n", [MessageId, FromPhone])
    catch
        Error:Reason ->
            io:format("   ❌ Error sending chat list update: ~p:~p~n", [Error, Reason])
    end.

%% @doc Enviar notificação push via FCM
%% Esta função é CRÍTICA para o sistema de entrega estilo WhatsApp
%% Envia push notification para acordar a app e permitir que envie ACK
send_fcm_notification(ToUserId, FromUserId, MessageId, Content) ->
    try
        io:format("📱 [FCM] Enviando push notification para ~p~n", [ToUserId]),
        io:format("   MessageId original: ~p (tipo: ~p)~n", [MessageId, erlang:is_integer(MessageId)]),
        
        %% Buscar nome do remetente para a notificação
        SenderName = case user_info_handler:get_user_from_db(FromUserId) of
            {ok, UserInfo} -> maps:get(<<"name">>, UserInfo, <<"Nova mensagem">>);
            _ -> <<"Nova mensagem">>
        end,
        
        %% Converter MessageId para binary se necessário
        MessageIdBin = case is_integer(MessageId) of
            true -> integer_to_binary(MessageId);
            false -> MessageId
        end,
        
        io:format("   MessageIdBin convertido: ~p~n", [MessageIdBin]),
        
        %% Enviar via FCM
        case fcm_sender:send_message_notification(ToUserId, FromUserId, MessageIdBin, Content, SenderName) of
            ok ->
                io:format("✅ [FCM] Push notification enviada com sucesso para ~p~n", [ToUserId]);
            {error, no_tokens} ->
                io:format("⚠️ [FCM] User ~p não tem tokens FCM registados~n", [ToUserId]);
            {error, FcmError} ->
                io:format("❌ [FCM] Erro ao enviar push: ~p~n", [FcmError])
        end
    catch
        ExcType:ExcReason ->
            io:format("❌ [FCM] Exceção ao enviar push: ~p:~p~n", [ExcType, ExcReason])
    end.
