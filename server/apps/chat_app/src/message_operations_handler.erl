%%%-------------------------------------------------------------------
%%% message_operations_handler.erl - Handler corrigido
%%%-------------------------------------------------------------------
-module(message_operations_handler).
-compile([{nowarn_function, update_reply_to_id, 2},
          {nowarn_function, send_reply_notification, 4},
          {nowarn_function, format_reply_message, 2}]).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    ?LOG_INFO("🔄 Message Operations Handler: ~p ~s", [Method, Path]),
    
    try
        handle_request(Method, Path, Req0, State)
    catch
        _:_Error:_Reason ->
            ?LOG_ERROR("❌ Exception in message_operations_handler"),
            send_json(Req0, 500, #{error => <<"internal_server_error">>}, State)
    end.

handle_request(<<"PATCH">>, <<"/api/messages/", _/binary>> = Path, Req0, State) ->
    case binary:match(Path, <<"/edit">>) of
        nomatch ->
            send_json(Req0, 404, #{error => <<"route_not_found">>}, State);
        _ ->
            MessageId = extract_message_id(Path),
            handle_edit_message(MessageId, Req0, State)
    end;

handle_request(<<"DELETE">>, <<"/api/messages/", _/binary>> = Path, Req0, State) ->
    MessageId = extract_message_id(Path),
    handle_delete_message(MessageId, Req0, State);

handle_request(<<"POST">>, <<"/api/messages/", _/binary>> = Path, Req0, State) ->
    MessageId = extract_message_id(Path),
    case binary:match(Path, <<"/reply">>) of
        nomatch ->
            send_json(Req0, 404, #{error => <<"route_not_found">>}, State);
        _ ->
            handle_reply_message(MessageId, Req0, State)
    end;

handle_request(<<"GET">>, <<"/api/messages/", _/binary>> = Path, Req0, State) ->
    MessageId = extract_message_id(Path),
    case binary:match(Path, <<"/history">>) of
        nomatch ->
            send_json(Req0, 404, #{error => <<"route_not_found">>}, State);
        _ ->
            handle_get_edit_history(MessageId, Req0, State)
    end;

handle_request(<<"POST">>, <<"/api/admin/messages/", _/binary>> = Path, Req0, State) ->
    MessageId = extract_message_id(Path),
    case binary:match(Path, <<"/recover">>) of
        nomatch ->
            send_json(Req0, 404, #{error => <<"route_not_found">>}, State);
        _ ->
            handle_admin_recover(MessageId, Req0, State)
    end;

handle_request(Method, Path, Req0, State) ->
    ?LOG_WARNING("❌ Route not found: ~p ~s", [Method, Path]),
    send_json(Req0, 404, #{error => <<"route_not_found">>}, State).

%% ======================
%% 3. RESPONDER MENSAGEM (POST /api/messages/:messageId/reply)
%% ======================
handle_reply_message(MessageId, Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        Data = jsx:decode(Body, [return_maps]),
        
        Content = maps:get(<<"content">>, Data, <<>>),
        SenderId = maps:get(<<"sender_id">>, Data, undefined),
        ReceiverId = maps:get(<<"receiver_id">>, Data, undefined),
        
        ?LOG_INFO("📨 Recebendo reply: messageId=~p, senderId=~p, receiverId=~p, content=~p", 
                 [MessageId, SenderId, ReceiverId, Content]),
        
        if
            Content =:= <<>> -> 
                send_json(Req1, 400, #{error => <<"content_required">>}, State);
            SenderId =:= undefined ->
                send_json(Req1, 401, #{error => <<"sender_id_required">>}, State);
            true ->
                %% 1. VERIFICAR SE MENSAGEM ORIGINAL EXISTE
                case message_repo:get_message_details(MessageId) of
                    {ok, OriginalMessage} ->
                        %% 2. DETERMINAR RECEIVER ID (CORREÇÃO CRÍTICA)
                        OriginalSenderId = maps:get(sender_id, OriginalMessage),
                        OriginalReceiverId = maps:get(receiver_id, OriginalMessage),
                        
                        SenderIdInt = binary_to_integer(SenderId),
                        
                        %% LÓGICA SIMPLIFICADA E CORRETA:
                        FinalReceiverId = case ReceiverId of
                            undefined ->
                                %% Se receiver não foi especificado, determinar baseado na mensagem original
                                if
                                    SenderIdInt =:= OriginalReceiverId ->
                                        integer_to_binary(OriginalSenderId);
                                    SenderIdInt =:= OriginalSenderId ->
                                        integer_to_binary(OriginalReceiverId);
                                    true ->
                                        %% Fallback
                                        integer_to_binary(OriginalReceiverId)
                                end;
                            _ ->
                                %% Usar o receiver especificado
                                ReceiverId
                        end,
                        
                        ?LOG_INFO("   Original: sender=~p, receiver=~p", 
                                 [OriginalSenderId, OriginalReceiverId]),
                        ?LOG_INFO("   Reply: sender=~p, receiver=~p", 
                                 [SenderId, FinalReceiverId]),
                        
                        %% 3. SALVAR MENSAGEM COM reply_to_id
                        case message_repo:save_reply_message(SenderId, FinalReceiverId, Content, MessageId) of
                            {ok, DbMessageId} ->
                                ?LOG_INFO("✅ Reply saved in DB: ~p", [DbMessageId]),
                                
                                %% 4. BUSCAR DADOS COMPLETOS DA RESPOSTA
                                case message_repo:get_message_details(integer_to_binary(DbMessageId)) of
                                    {ok, ReplyMessage} ->
                                        %% 5. OBTER DADOS DA MENSAGEM ORIGINAL PARA INCLUIR NO REPLY
                                        OriginalText = maps:get(content, OriginalMessage, <<"Mensagem original">>),
                                        OriginalSenderIdBin = integer_to_binary(OriginalSenderId),
                                        
                                        %% Determinar nome do remetente original
                                        {ok, OriginalSenderName} = case message_repo:get_user_name(OriginalSenderIdBin) of
                                            {ok, Name} -> {ok, Name};
                                            _ -> {ok, <<"Usuário">>}
                                        end,
                                        
                                        ReplyToSenderName = if
                                            SenderIdInt =:= OriginalSenderId -> <<"Eu">>;
                                            true -> OriginalSenderName
                                        end,
                                        
                                        %% 6. CRIAR ESTRUTURA DE DADOS PARA O REPLY
                                        ReplyData = #{
                                            <<"id">> => integer_to_binary(DbMessageId),
                                            <<"sender_id">> => SenderId,
                                            <<"receiver_id">> => FinalReceiverId,
                                            <<"content">> => Content,
                                            <<"sent_at">> => maps:get(sent_at, ReplyMessage),
                                            <<"status">> => <<"sent">>,
                                            <<"reply_to_id">> => MessageId,
                                            <<"reply_to_text">> => OriginalText,
                                            <<"reply_to_sender_name">> => ReplyToSenderName,
                                            <<"reply_to_sender_id">> => OriginalSenderIdBin
                                        },
                                        
                                        %% 7. ENVIAR PARA O DESTINATÁRIO (STATUS = delivered)
                                        MessageToReceiver = #{
                                            <<"type">> => <<"message">>, %% Tipo normal!
                                            <<"from">> => SenderId,
                                            <<"to">> => FinalReceiverId,
                                            <<"content">> => Content,
                                            <<"timestamp">> => erlang:system_time(second),
                                            <<"message_id">> => integer_to_binary(DbMessageId),
                                            <<"db_message_id">> => integer_to_binary(DbMessageId),
                                            <<"status">> => <<"delivered">>,
                                            %% INFORMAÇÕES DE REPLY
                                            <<"reply_to_id">> => MessageId,
                                            <<"reply_to_text">> => OriginalText,
                                            <<"reply_to_sender_name">> => ReplyToSenderName,
                                            <<"reply_to_sender_id">> => OriginalSenderIdBin,
                                            <<"should_increase_unread">> => true
                                        },
                                        
                                        ?LOG_INFO("   Enviando para receiver ~p", [FinalReceiverId]),

                                        %% ✅ Mesma verificação de presença usada no fluxo normal
                                        IsUserReallyOnline = case presence_manager:is_user_online(FinalReceiverId) of
                                            {ok, true} -> true;
                                            _ -> false
                                        end,
                                        IsWsAlive = user_session:is_websocket_alive(FinalReceiverId),
                                        ?LOG_INFO("   🔍 WebSocket alive: ~p, Usuário realmente online: ~p", [IsWsAlive, IsUserReallyOnline]),

                                        case IsWsAlive andalso IsUserReallyOnline of
                                            true ->
                                                case user_session:send_message(SenderId, FinalReceiverId, MessageToReceiver) of
                                                    ok ->
                                                %% ✅ Atualizar status no banco
                                                message_repo:mark_message_delivered(DbMessageId),
                                                
                                                %% ✅ ENVIAR EVENTO DE DELIVERED PARA O REMETENTE
                                                %% (mesmo comportamento do fluxo normal via WebSocket)
                                                DeliveryMsg = #{
                                                    <<"type">> => <<"message_delivered">>,
                                                    <<"message_id">> => integer_to_binary(DbMessageId),
                                                    <<"db_message_id">> => DbMessageId,
                                                    <<"status">> => <<"delivered">>,
                                                    <<"delivered_at">> => erlang:system_time(second)
                                                },
                                                %% from = destinatário, to = remetente (mesma convenção de message_router)
                                                user_session:send_message(FinalReceiverId, SenderId, DeliveryMsg),
                                                
                                                %% 8. ENVIAR CONFIRMAÇÃO PARA O REMETENTE (STATUS = delivered)
                                                MessageToSender = #{
                                                    <<"type">> => <<"message">>,
                                                    <<"from">> => SenderId,
                                                    <<"to">> => FinalReceiverId,
                                                    <<"content">> => Content,
                                                    <<"timestamp">> => erlang:system_time(second),
                                                    <<"message_id">> => integer_to_binary(DbMessageId),
                                                    <<"db_message_id">> => integer_to_binary(DbMessageId),
                                                    <<"status">> => <<"delivered">>,
                                                    %% MESMAS INFORMAÇÕES DE REPLY
                                                    <<"reply_to_id">> => MessageId,
                                                    <<"reply_to_text">> => OriginalText,
                                                    <<"reply_to_sender_name">> => ReplyToSenderName,
                                                    <<"reply_to_sender_id">> => OriginalSenderIdBin,
                                                    <<"should_increase_unread">> => false
                                                },
                                                
                                                ?LOG_INFO("   Enviando confirmação para sender ~p", [SenderId]),
                                                user_session:send_message(SenderId, SenderId, MessageToSender),
                                                
                                                %% 9. ATUALIZAR CHAT LIST
                                                send_chat_list_edit_update(SenderId, FinalReceiverId, Content, integer_to_binary(DbMessageId)),
                                                
                                                ?LOG_INFO("✅ Reply processado com sucesso"),
                                                
                                                %% 10. RETORNAR RESPOSTA (STATUS = delivered)
                                                ReplyDataDelivered = ReplyData#{
                                                    <<"status">> => <<"delivered">>
                                                },
                                                send_json(Req1, 201, #{
                                                    success => true,
                                                    message => <<"Reply sent successfully">>,
                                                    reply_message => ReplyDataDelivered
                                                }, State);
                                                    {error, user_offline} ->
                                                        %% Destinatário offline: não falhar. Guardar offline com metadados de reply.
                                                        ?LOG_WARNING("ℹ️ Receiver ~p offline ao enviar reply. Guardando como 'sent' e respondendo sucesso.", [FinalReceiverId]),
                                                        MessageOffline = MessageToReceiver#{
                                                            <<"status">> => <<"sent">>,
                                                            <<"should_increase_unread">> => true
                                                        },
                                                        message_router:store_offline_message(FinalReceiverId, MessageOffline),

                                                        MessageToSender2 = #{
                                                            <<"type">> => <<"message">>,
                                                            <<"from">> => SenderId,
                                                            <<"to">> => FinalReceiverId,
                                                            <<"content">> => Content,
                                                            <<"timestamp">> => erlang:system_time(second),
                                                            <<"message_id">> => integer_to_binary(DbMessageId),
                                                            <<"db_message_id">> => integer_to_binary(DbMessageId),
                                                            <<"status">> => <<"sent">>,
                                                            <<"reply_to_id">> => MessageId,
                                                            <<"reply_to_text">> => OriginalText,
                                                            <<"reply_to_sender_name">> => ReplyToSenderName,
                                                            <<"reply_to_sender_id">> => OriginalSenderIdBin,
                                                            <<"should_increase_unread">> => false
                                                        },
                                                        user_session:send_message(SenderId, SenderId, MessageToSender2),
                                                        send_chat_list_edit_update(SenderId, FinalReceiverId, Content, integer_to_binary(DbMessageId)),
                                                        send_json(Req1, 201, #{
                                                            success => true,
                                                            message => <<"Reply queued (receiver offline)">>,
                                                            reply_message => ReplyData
                                                        }, State);
                                                    {error, Error} ->
                                                        ?LOG_ERROR("❌ Failed to send reply: ~p", [Error]),
                                                        send_json(Req1, 500, #{error => <<"failed_to_send_reply">>}, State)
                                                end;
                                            false ->
                                                %% ✅ Offline real (mesmo que o WS responda): guardar como sent
                                                ?LOG_WARNING("ℹ️ Receiver ~p offline (presence/heartbeat). Guardando reply como 'sent'.", [FinalReceiverId]),
                                                MessageOffline = MessageToReceiver#{
                                                    <<"status">> => <<"sent">>,
                                                    <<"should_increase_unread">> => true
                                                },
                                                message_router:store_offline_message(FinalReceiverId, MessageOffline),

                                                MessageToSender2 = #{
                                                    <<"type">> => <<"message">>,
                                                    <<"from">> => SenderId,
                                                    <<"to">> => FinalReceiverId,
                                                    <<"content">> => Content,
                                                    <<"timestamp">> => erlang:system_time(second),
                                                    <<"message_id">> => integer_to_binary(DbMessageId),
                                                    <<"db_message_id">> => integer_to_binary(DbMessageId),
                                                    <<"status">> => <<"sent">>,
                                                    <<"reply_to_id">> => MessageId,
                                                    <<"reply_to_text">> => OriginalText,
                                                    <<"reply_to_sender_name">> => ReplyToSenderName,
                                                    <<"reply_to_sender_id">> => OriginalSenderIdBin,
                                                    <<"should_increase_unread">> => false
                                                },
                                                user_session:send_message(SenderId, SenderId, MessageToSender2),
                                                send_chat_list_edit_update(SenderId, FinalReceiverId, Content, integer_to_binary(DbMessageId)),
                                                send_json(Req1, 201, #{
                                                    success => true,
                                                    message => <<"Reply queued (receiver offline)">>,
                                                    reply_message => ReplyData
                                                }, State)
                                        end;
                                    {error, _} ->
                                        send_json(Req1, 500, #{error => <<"failed_to_get_reply">>}, State)
                                end;
                            {error, Error} ->
                                ?LOG_ERROR("❌ Error saving reply: ~p", [Error]),
                                send_json(Req1, 500, #{error => <<"failed_to_save_reply">>}, State)
                        end;
                    {error, not_found} ->
                        send_json(Req1, 404, #{error => <<"original_message_not_found">>}, State);
                    {error, _} ->
                        send_json(Req1, 500, #{error => <<"failed_to_get_original_message">>}, State)
                end
        end
    catch
        Exception:Reason:Stacktrace ->
            ?LOG_ERROR("❌ Exception in reply_message: ~p:~p~nStacktrace: ~p", 
                      [Exception, Reason, Stacktrace]),
            send_json(Req0, 500, #{error => <<"server_error">>}, State)
    end.

%% ======================
%% 1. EDITAR MENSAGEM (PATCH /api/messages/:messageId/edit)
%% ======================
handle_edit_message(MessageId, Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        Data = jsx:decode(Body, [return_maps]),
        
        Content = maps:get(<<"content">>, Data, <<>>),
        UserId = maps:get(<<"user_id">>, Data, undefined),
        
        if
            Content =:= <<>> -> 
                send_json(Req1, 400, #{error => <<"content_required">>}, State);
            UserId =:= undefined ->
                send_json(Req1, 401, #{error => <<"user_id_required">>}, State);
            true ->
                %% ✅ VERIFICAR SE USUÁRIO PODE EDITAR
                ?LOG_INFO("🔍 Checking edit permissions for message ~p by user ~p", [MessageId, UserId]),
                %% Na função handle_edit_message, linha ~331:
                case message_repo:can_edit_message(MessageId, UserId) of
                    {ok, can_edit} ->  % ← Este é o padrão que está falhando
                        ?LOG_INFO("✅ User can edit message: ~p", [can_edit]),
                        %% ✅ OBTER MENSAGEM ORIGINAL
                        case message_repo:get_message_details(MessageId) of
                            {ok, OriginalMessage} ->
                                OriginalContent = maps:get(content, OriginalMessage),
                                
                                %% ✅ SALVAR NO HISTÓRICO
                                save_edit_history(MessageId, OriginalContent, Content, UserId),
                                
                                %% ✅ ATUALIZAR MENSAGEM
                                case update_message_content(MessageId, Content) of
                                    ok ->
                                        %% OBTER MENSAGEM ATUALIZADA
                                        case message_repo:get_message_details(MessageId) of
                                            {ok, UpdatedMessage} ->
                                                
                                                %% NOTIFICAR DESTINATÁRIO
                                                notify_message_edited(UpdatedMessage),
                                                
                                                %% ATUALIZAR CHAT LIST PARA AMBOS COM ACTION=edit_message
                                                SenderIdBin = integer_to_binary(maps:get(sender_id, UpdatedMessage)),
                                                ReceiverIdBin = integer_to_binary(maps:get(receiver_id, UpdatedMessage)),
                                                Content = maps:get(content, UpdatedMessage),
                                                UpdatedMessageId = integer_to_binary(maps:get(id, UpdatedMessage)),
                                                
                                                % Para o remetente (deve mostrar a mensagem editada)
                                                send_chat_list_edit_update(SenderIdBin, ReceiverIdBin, Content, UpdatedMessageId),
                                                
                                                % Para o destinatário (deve mostrar a mensagem editada)  
                                                send_chat_list_edit_update(ReceiverIdBin, SenderIdBin, Content, UpdatedMessageId),
                                                
                                                send_json(Req1, 200, #{
                                                    success => true,
                                                    message => <<"Message edited successfully">>,
                                                    edited_message => format_message(UpdatedMessage)
                                                }, State);
                                            {error, _} ->
                                                send_json(Req1, 500, #{error => <<"failed_to_get_updated_message">>}, State)
                                        end;
                                    {error, _} ->
                                        send_json(Req1, 500, #{error => <<"failed_to_update_message">>}, State)
                                end;
                            {error, _} ->
                                send_json(Req1, 404, #{error => <<"message_not_found">>}, State)
                        end;
                    {error, not_authorized} ->
                        send_json(Req1, 403, #{error => <<"not_authorized_to_edit">>}, State);
                    {error, edit_time_expired} ->
                        send_json(Req1, 400, #{error => <<"edit_time_expired">>}, State);
                    {error, already_deleted} ->
                        send_json(Req1, 410, #{error => <<"message_already_deleted">>}, State);
                    {error, _} ->
                        send_json(Req1, 500, #{error => <<"internal_error">>}, State)
                end
        end
    catch
        %% CORREÇÃO: Use '_' em vez de variáveis nomeadas
        Class:Reason:Stacktrace ->
            ?LOG_ERROR("❌ Exception in edit_message: ~p:~p~nStacktrace: ~p", 
                      [Class, Reason, Stacktrace]),
            send_json(Req0, 400, #{error => <<"invalid_request">>}, State)
    end.

%% ======================
%% 2. APAGAR MENSAGEM (DELETE /api/messages/:messageId/delete)
%% ======================
handle_delete_message(MessageId, Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        Data = jsx:decode(Body, [return_maps]),
        
        UserId = maps:get(<<"user_id">>, Data, undefined),
        Reason = maps:get(<<"reason">>, Data, <<"user_deleted">>),
        
        if
            UserId =:= undefined ->
                send_json(Req1, 401, #{error => <<"user_id_required">>}, State);
            true ->
                %% ✅ VERIFICAR SE USUÁRIO PODE DELETAR
                case message_repo:can_delete_message(MessageId, UserId) of
                    {ok, can_delete} ->
                        %% ✅ OBTER MENSAGEM
                        case message_repo:get_message_details(MessageId) of
                            {ok, Message} ->
                                OriginalContent = maps:get(content, Message),
                                SenderId = maps:get(sender_id, Message),
                                ReceiverId = maps:get(receiver_id, Message),
                                
                                %% ✅ SALVAR NO LOG DE DELEÇÃO
                                save_delete_log(MessageId, UserId, OriginalContent, Reason),
                                
                                %% SOFT DELETE
                                case soft_delete_message(MessageId, UserId, Reason) of
                                    ok ->
                                        ?LOG_INFO("✅ Message deleted: ~p by user ~p", [MessageId, UserId]),
                                        
                                        %% NOTIFICAR AMBOS OS USUÁRIOS COM QUEM DELETOU CORRETAMENTE
                                        notify_message_deleted(MessageId, UserId, 
                                                              integer_to_binary(SenderId), integer_to_binary(ReceiverId), Reason),
                                        
                                        %% ATUALIZAR CHAT LIST PARA AMBOS COM ACTION=delete_message
                                        send_chat_list_delete_update(integer_to_binary(SenderId), integer_to_binary(ReceiverId), 
                                                                   MessageId, UserId),
                                        
                                        %% TAMBÉM ENVIAR PARA O DESTINATÁRIO (IMPORTANTE!)
                                        send_chat_list_delete_update(integer_to_binary(ReceiverId), integer_to_binary(SenderId), 
                                                                   MessageId, UserId),
                                        
                                        send_json(Req1, 200, #{
                                            success => true,
                                            message => <<"Message deleted successfully">>,
                                            deleted_message => #{
                                                id => MessageId,
                                                deleted_by => UserId,
                                                delete_reason => Reason,
                                                deleted_at => erlang:system_time(second)
                                            }
                                        }, State);
                                    {error, _} ->
                                        send_json(Req1, 500, #{error => <<"failed_to_delete_message">>}, State)
                                end;
                            {error, _} ->
                                send_json(Req1, 404, #{error => <<"message_not_found">>}, State)
                        end;
                    {error, not_authorized} ->
                        send_json(Req1, 403, #{error => <<"not_authorized_to_delete">>}, State);
                    {error, delete_time_expired} ->
                        send_json(Req1, 400, #{error => <<"delete_time_expired">>}, State);
                    {error, already_deleted} ->
                        send_json(Req1, 410, #{error => <<"message_already_deleted">>}, State);
                    {error, _} ->
                        send_json(Req1, 500, #{error => <<"internal_error">>}, State)
                end
        end
    catch
        %% CORREÇÃO: Capturar exceção real para debug
        Class:Exception:Stacktrace ->
            ?LOG_ERROR("❌ Exception in delete_message: ~p:~p~nStacktrace: ~p", 
                      [Class, Exception, Stacktrace]),
            send_json(Req0, 500, #{error => <<"internal_error">>, details => io_lib:format("~p:~p~n~p", [Class, Exception, Stacktrace])}, State)
    end.

%% ======================
%% 4. HISTÓRICO DE EDIÇÕES (GET /api/messages/:messageId/history)
%% ======================
handle_get_edit_history(MessageId, Req0, State) ->
    try
        Token = cowboy_req:header(<<"authorization">>, Req0),
        
        case Token of
            undefined ->
                send_json(Req0, 401, #{error => <<"unauthorized">>}, State);
            _ ->
                case message_repo:get_edit_history(MessageId) of
                    {ok, History} ->
                        send_json(Req0, 200, #{
                            message_id => MessageId,
                            edit_history => format_edit_history(History)
                        }, State);
                    {error, not_found} ->
                        send_json(Req0, 404, #{error => <<"message_not_found">>}, State);
                    {error, _Reason} ->
                        send_json(Req0, 500, #{error => <<"internal_error">>}, State)
                end
        end
    catch
        %% CORREÇÃO: Use '_' em vez de variáveis nomeadas
        _:_ ->
            ?LOG_ERROR("❌ Exception in get_edit_history"),
            send_json(Req0, 400, #{error => <<"invalid_request">>}, State)
    end.

%% ======================
%% 5. RECUPERAR MENSAGEM (ADMIN) (POST /api/admin/messages/:messageId/recover)
%% ======================
handle_admin_recover(MessageId, Req0, State) ->
    try
        {ok, Body, Req1} = cowboy_req:read_body(Req0),
        Data = jsx:decode(Body, [return_maps]),
        
        AdminId = maps:get(<<"admin_id">>, Data, undefined),
        
        if
            AdminId =:= undefined ->
                send_json(Req1, 401, #{error => <<"admin_id_required">>}, State);
            true ->
                case is_admin(AdminId) of
                    true ->
                        case recover_message_db(MessageId, AdminId) of
                            {ok, RecoveredMessage} ->
                                ?LOG_INFO("✅ Message recovered by admin ~p: ~p", 
                                         [AdminId, MessageId]),
                                
                                send_json(Req1, 200, #{
                                    success => true,
                                    message => <<"Message recovered successfully">>,
                                    recovered_message => format_message(RecoveredMessage)
                                }, State);
                            {error, not_found} ->
                                send_json(Req1, 404, #{error => <<"message_not_found">>}, State);
                            {error, not_deleted} ->
                                send_json(Req1, 400, #{error => <<"message_not_deleted">>}, State);
                            {error, _Reason} ->
                                send_json(Req1, 500, #{error => <<"internal_error">>}, State)
                        end;
                    false ->
                        send_json(Req1, 403, #{error => <<"admin_required">>}, State)
                end
        end
    catch
        %% CORREÇÃO: Use '_' em vez de variáveis nomeadas
        _:_ ->
            ?LOG_ERROR("❌ Exception in admin_recover"),
            send_json(Req0, 400, #{error => <<"invalid_request">>}, State)
    end.

%% ======================
%% FUNÇÕES AUXILIARES
%% ======================

extract_message_id(<<"/api/messages/", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>) of
        [MessageId, _] -> MessageId;
        [MessageId] -> MessageId
    end;
extract_message_id(<<"/api/admin/messages/", Rest/binary>>) ->
    case binary:split(Rest, <<"/">>) of
        [MessageId, _] -> MessageId;
        [MessageId] -> MessageId
    end;
extract_message_id(_) ->
    <<>>.

%% ======================
%% FUNÇÕES DE BANCO DE DADOS REAIS
%% ======================

%% Atualizar reply_to_id de uma mensagem
update_reply_to_id(MessageId, ReplyToId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages SET reply_to_id = $1 WHERE id = $2",
        case epgsql:equery(Conn, Sql, [binary_to_integer(ReplyToId), MessageId]) of
            {ok, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).

%% Salvar histórico de edição
save_edit_history(MessageId, OriginalContent, NewContent, UserId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "INSERT INTO message_edit_history 
               (message_id, original_content, edited_content, edited_by) 
               VALUES ($1, $2, $3, $4)",
        MsgIdInt = binary_to_integer(MessageId),
        UserIdInt = binary_to_integer(UserId),
        epgsql:equery(Conn, Sql, [MsgIdInt, OriginalContent, NewContent, UserIdInt])
    end).

%% Atualizar conteúdo da mensagem
update_message_content(MessageId, NewContent) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages 
               SET content = $1, is_edited = true, 
                   edit_count = edit_count + 1, edited_at = NOW()
               WHERE id = $2",
        MsgIdInt = binary_to_integer(MessageId),
        case epgsql:equery(Conn, Sql, [NewContent, MsgIdInt]) of
            {ok, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).

%% Salvar log de deleção
save_delete_log(MessageId, UserId, Content, Reason) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "INSERT INTO message_delete_log 
               (message_id, deleted_by, original_content, delete_reason) 
               VALUES ($1, $2, $3, $4)",
        MsgIdInt = binary_to_integer(MessageId),
        UserIdInt = binary_to_integer(UserId),
        epgsql:equery(Conn, Sql, [MsgIdInt, UserIdInt, Content, Reason])
    end).

%% Soft delete de mensagem
soft_delete_message(MessageId, UserId, Reason) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages 
               SET is_deleted = true, deleted_by = $1,
                   delete_reason = $2, deleted_at = NOW()
               WHERE id = $3",
        MsgIdInt = binary_to_integer(MessageId),
        UserIdInt = binary_to_integer(UserId),
        case epgsql:equery(Conn, Sql, [UserIdInt, Reason, MsgIdInt]) of
            {ok, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).

%% Recuperar mensagem (admin)
recover_message_db(MessageId, AdminId) ->
    db_pool:with_connection(fun(Conn) ->
        MsgIdInt = binary_to_integer(MessageId),
        %% CORREÇÃO: Remover variável não usada ou usar underscore
        _AdminIdInt = binary_to_integer(AdminId),
        
        Sql = "UPDATE messages 
               SET is_deleted = false, deleted_by = NULL,
                   delete_reason = NULL, deleted_at = NULL
               WHERE id = $1 AND deleted_by = $2",
        
        case epgsql:equery(Conn, Sql, [MsgIdInt, _AdminIdInt]) of
            {ok, _} ->
                message_repo:get_message_details(MessageId);
            {error, Error} ->
                {error, Error}
        end
    end).

%% ======================

%% Enviar notificação específica de resposta (sem mostrar pop-up verde para remetente)
send_reply_notification(ReplyMessage, OriginalMessageId, SenderId, ReceiverId) ->
    try
        %% ✅ OBTER INFORMAÇÕES DA MENSAGEM ORIGINAL
        case message_repo:get_message_details(OriginalMessageId) of
            {ok, OriginalMessage} ->
                OriginalText = maps:get(<<"content">>, OriginalMessage, <<>>),
                OriginalSenderId = maps:get(<<"sender_id">>, OriginalMessage, <<"unknown">>),
                
                %% ✅ LÓGICA CORRETA: Mostrar "Eu" apenas para o destinatário da resposta
                %% Se a mensagem original foi enviada por quem está recebendo a notificação, mostrar "Eu"
                OriginalSenderName = case OriginalSenderId of
                    ReceiverId -> <<"Eu">>;
                    _ -> 
                        case message_repo:get_user_name(OriginalSenderId) of
                            {ok, Name} -> Name;
                            _ -> <<"Usuário">>
                        end
                end,
                
                MessageIdBinary = integer_to_binary(maps:get(id, ReplyMessage)),
                Content = maps:get(content, ReplyMessage),
                
                %% Notificação para o destinatário (sem should_increase_unread para não mostrar pop-up verde)
                Notification = #{
                    <<"type">> => <<"message_reply">>,
                    <<"message_id">> => MessageIdBinary,
                    <<"original_message_id">> => OriginalMessageId,
                    <<"sender_id">> => SenderId,
                    <<"receiver_id">> => ReceiverId,
                    <<"content">> => Content,
                    <<"reply_to_text">> => OriginalText,
                    <<"reply_to_sender_name">> => OriginalSenderName,
                    <<"timestamp">> => erlang:system_time(second),
                    <<"should_increase_unread">> => false  %% Importante: não mostrar notificação verde
                },
                
                user_session:send_message(SenderId, ReceiverId, Notification),
                ?LOG_INFO("📡 Reply notification sent: ~p -> ~p (reply_to: ~s)", [SenderId, ReceiverId, OriginalSenderName]);
            {error, _} ->
                ?LOG_ERROR("❌ Failed to get original message for reply notification")
        end
    catch
        _:_ ->
            ?LOG_ERROR("❌ Error sending reply notification")
    end.

%% Notificar edição
notify_message_edited(Message) ->
    try
        MessageId = integer_to_binary(maps:get(id, Message)),
        SenderId = integer_to_binary(maps:get(sender_id, Message)),
        ReceiverId = integer_to_binary(maps:get(receiver_id, Message)),
        Content = maps:get(content, Message),
        
        Notification = #{
            <<"type">> => <<"message_edited">>,
            <<"message_id">> => MessageId,
            <<"sender_id">> => SenderId,
            <<"receiver_id">> => ReceiverId,
            <<"content">> => Content
            % ✅ NÃO ENVIAR TIMESTAMP EM EDIÇÕES - NÃO DEVE MOVER CHAT!
            % <<"timestamp">> => erlang:system_time(second)
        },
        
        % Enviar para destinatário
        user_session:send_message(SenderId, ReceiverId, Notification),
        % Enviar para remetente (para atualizar UI dele também)
        user_session:send_message(SenderId, SenderId, Notification),
        
        ?LOG_INFO("📡 Edit notification sent: ~p -> ~p and ~p -> ~p", [SenderId, ReceiverId, SenderId, SenderId])
    catch
        %% CORREÇÃO: Use '_' em vez de variáveis nomeadas
        _:_ ->
            ?LOG_ERROR("❌ Error sending edit notification")
    end.

%% Notificar deleção
notify_message_deleted(MessageId, DeletedBy, SenderId, ReceiverId, Reason) ->
    try
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
        ?LOG_INFO("📡 Delete notification sent: ~p -> ~p and ~p -> ~p (deleted_by: ~p)", [SenderId, ReceiverId, SenderId, SenderId, DeletedBy])
    catch
        %% CORREÇÃO: Use '_' em vez de variáveis nomeadas
        _:_ ->
            ?LOG_ERROR("❌ Error sending delete notification")
    end.

%% ======================
%% FUNÇÕES DE FORMATAÇÃO
%% ======================

format_reply_message(Message, OriginalMessageId) ->
    %% Obter detalhes da mensagem original para incluir o sender_id
    case message_repo:get_message_details(OriginalMessageId) of
        {ok, OriginalMessage} ->
            OriginalSenderId = maps:get(<<"sender_id">>, OriginalMessage, <<"unknown">>),
            #{
                <<"id">> => integer_to_binary(maps:get(id, Message)),
                <<"sender_id">> => integer_to_binary(maps:get(sender_id, Message)),
                <<"receiver_id">> => integer_to_binary(maps:get(receiver_id, Message)),
                <<"content">> => maps:get(content, Message),
                <<"sent_at">> => maps:get(sent_at, Message),
                <<"status">> => maps:get(status, Message, <<"sent">>),
                <<"reply_to_id">> => OriginalMessageId,
                <<"original_message_id">> => OriginalMessageId,
                <<"reply_to_sender_id">> => integer_to_binary(OriginalSenderId)
            };
        {error, _} ->
            #{
                <<"id">> => integer_to_binary(maps:get(id, Message)),
                <<"sender_id">> => integer_to_binary(maps:get(sender_id, Message)),
                <<"receiver_id">> => integer_to_binary(maps:get(receiver_id, Message)),
                <<"content">> => maps:get(content, Message),
                <<"sent_at">> => maps:get(sent_at, Message),
                <<"status">> => maps:get(status, Message, <<"sent">>),
                <<"reply_to_id">> => OriginalMessageId,
                <<"original_message_id">> => OriginalMessageId,
                <<"reply_to_sender_id">> => <<"unknown">>
            }
    end.

format_message(Message) ->
    try
        io:format("🔍 DEBUG format_message: Message=~p~n", [Message]),
        Id = maps:get(id, Message),
        io:format("🔍 DEBUG format_message: Id=~p~n", [Id]),
        SenderId = maps:get(sender_id, Message),
        io:format("🔍 DEBUG format_message: SenderId=~p~n", [SenderId]),
        ReceiverId = maps:get(receiver_id, Message),
        io:format("🔍 DEBUG format_message: ReceiverId=~p~n", [ReceiverId]),
        Content = maps:get(content, Message),
        SentAt = maps:get(sent_at, Message),
        Status = maps:get(status, Message, <<"sent">>),
        IsEdited = maps:get(is_edited, Message, false),
        EditCount = maps:get(edit_count, Message, 0),
        ReplyToId = maps:get(reply_to_id, Message, undefined),
        io:format("🔍 DEBUG format_message: ReplyToId=~p~n", [ReplyToId]),
        
        #{
            <<"id">> => integer_to_binary(Id),
            <<"sender_id">> => integer_to_binary(SenderId),
            <<"receiver_id">> => integer_to_binary(ReceiverId),
            <<"content">> => Content,
            <<"sent_at">> => SentAt,
            <<"status">> => Status,
            <<"is_edited">> => IsEdited,
            <<"edit_count">> => EditCount,
            <<"reply_to_id">> => case ReplyToId of
                                   undefined -> null;
                                   null -> null;
                                   ReplyToId -> integer_to_binary(ReplyToId)
                                 end,
            <<"deleted_by">> => case maps:get(deleted_by, Message, undefined) of
                                   undefined -> null;
                                   null -> null;
                                   DelBy -> integer_to_binary(DelBy)
                                 end,
            <<"deleted_at">> => case maps:get(deleted_at, Message, undefined) of
                                   undefined -> null;
                                   null -> null;
                                   DelAt -> DelAt
                                 end
        }
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Exception in format_message: ~p:~p~nStacktrace: ~p", 
                      [Class, Reason, Stacktrace]),
            throw({error, format_failed})
    end.

format_edit_history(History) ->
    lists:map(fun(Edit) ->
        #{
            <<"id">> => integer_to_binary(maps:get(id, Edit)),
            <<"original_content">> => maps:get(original_content, Edit),
            <<"edited_content">> => maps:get(edited_content, Edit),
            <<"edited_by">> => integer_to_binary(maps:get(edited_by, Edit)),
            <<"editor_name">> => maps:get(editor_name, Edit, <<"Unknown">>),
            <<"edited_at">> => maps:get(edited_at, Edit)
        }
    end, History).
%% FUNÇÕES AUXILIARES
%% ======================

is_admin(UserId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            UserIdInt = binary_to_integer(UserId),
            Sql = "SELECT EXISTS(SELECT 1 FROM user_roles ur 
                     JOIN roles r ON ur.role_id = r.id 
                     WHERE ur.user_id = $1 AND r.name = 'Admin')",
            
            case epgsql:equery(Conn, Sql, [UserIdInt]) of
                {ok, _, [{true}]} -> true;
                {ok, _, [{false}]} -> false;
                {error, _} -> false
            end
            
        catch
            _:_ -> false
        end
    end).

%% Enviar atualização para chat list page de deleção (com action=delete_message)
send_chat_list_delete_update(SenderId, ReceiverId, MessageId, DeletedBy) ->
    io:format("🔍 DEBUG send_chat_list_delete_update: INÍCIO - SenderId=~p, ReceiverId=~p, MessageId=~p, DeletedBy=~p~n", [SenderId, ReceiverId, MessageId, DeletedBy]),
    try
        %% ✅ VERIFICAR SE É A ÚLTIMA MENSAGEM DO CHAT ANTES DE ATUALIZAR
        MessageIdInt = binary_to_integer(MessageId),
        io:format("🔍 DEBUG send_chat_list_delete_update: Verificando se mensagem ~p é a última do chat~n", [MessageIdInt]),
        
        case message_repo:is_last_message_in_chat(SenderId, ReceiverId, MessageIdInt) of
            {ok, true} ->
                io:format("🔍 DEBUG send_chat_list_delete_update: ✅ É a última mensagem - ATUALIZANDO chat list~n", []),
                %% É a última mensagem - pode atualizar chat list
                %% Personalizar mensagem baseado em quem deletou
                Content = case DeletedBy of
                    SenderId -> <<"⊗ Eliminou esta mensagem">>;  % EU apaguei
                    _ -> <<"⊗ Esta mensagem foi apagada">>  % OUTRO apagou
                end,
                
                %% Notificação para o remetente (atualizar chat list dele)
                SenderUpdate = #{
                    <<"type">> => <<"chat_list_update">>,
                    <<"message_id">> => MessageId,  % ← JÁ É BINÁRIO!
                    <<"from">> => SenderId,
                    <<"to">> => ReceiverId,
                    <<"content">> => Content,
                    % ✅ NÃO ENVIAR TIMESTAMP EM DELEÇÕES - NÃO DEVE MOVER CHAT!
                    % <<"timestamp">> => erlang:system_time(second),
                    <<"action">> => <<"delete_message">>,
                    <<"deleted_by">> => DeletedBy
                },
                io:format("🔍 DEBUG send_chat_list_delete_update: Enviando SenderUpdate=~p~n", [SenderUpdate]),
                user_session:send_message(SenderId, ReceiverId, SenderUpdate),
                io:format("🔍 DEBUG send_chat_list_delete_update: SenderUpdate enviado com sucesso~n", []),
                
                %% Notificação para o destinatário (atualizar chat list dele)
                ReceiverUpdate = #{
                    <<"type">> => <<"chat_list_update">>,
                    <<"message_id">> => MessageId,  % ← JÁ É BINÁRIO!
                    <<"from">> => SenderId,
                    <<"to">> => ReceiverId,
                    <<"content">> => Content,
                    % ✅ NÃO ENVIAR TIMESTAMP EM DELEÇÕES - NÃO DEVE MOVER CHAT!
                    % <<"timestamp">> => erlang:system_time(second),
                    <<"action">> => <<"delete_message">>,
                    <<"deleted_by">> => DeletedBy
                },
                user_session:send_message(ReceiverId, SenderId, ReceiverUpdate),
                
                ?LOG_INFO("   📋 Chat list delete update sent for message ~p", [MessageId]);
            
            {ok, false} ->
                io:format("🔍 DEBUG send_chat_list_delete_update: 🚫 Não é a última mensagem - NÃO ATUALIZANDO chat list~n", []),
                %% Não é a última mensagem - não atualizar chat list
                ?LOG_INFO("   🚫 Message ~p is not the last message - skipping chat list update", [MessageId]);
            
            {error, Error} ->
                io:format("🔍 DEBUG send_chat_list_delete_update: ❌ Erro ao verificar última mensagem: ~p~n", [Error]),
                ?LOG_ERROR("   ❌ Error checking if message is last: ~p", [Error])
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Exception in send_chat_list_delete_update: ~p:~p~nStacktrace: ~p", 
                      [Class, Reason, Stacktrace]),
            ?LOG_ERROR("   ❌ Failed to send chat list delete update")
    end.

%% Enviar atualização para chat list page de edição (com action=edit_message)
send_chat_list_edit_update(SenderId, ReceiverId, Content, MessageId) ->
    try
        %% ✅ VERIFICAR SE É A ÚLTIMA MENSAGEM DO CHAT ANTES DE ATUALIZAR
        MessageIdInt = binary_to_integer(MessageId),
        case message_repo:is_last_message_in_chat(SenderId, ReceiverId, MessageIdInt) of
            {ok, true} ->
                %% É a última mensagem - pode atualizar chat list
                %% Notificação para o remetente (atualizar chat list dele)
                Update = #{
                    <<"type">> => <<"chat_list_update">>,
                    <<"message_id">> => MessageId,  % ← JÁ É BINÁRIO!
                    <<"from">> => SenderId,
                    <<"to">> => ReceiverId,
                    <<"content">> => Content,
                    % ✅ NÃO ENVIAR TIMESTAMP EM EDIÇÕES - NÃO DEVE MOVER CHAT!
                    % <<"timestamp">> => erlang:system_time(second),
                    <<"action">> => <<"edit_message">>
                },
                io:format("🔍 DEBUG send_chat_list_edit_update: Enviando Update=~p~n", [Update]),
                user_session:send_message(ReceiverId, SenderId, Update),
                user_session:send_message(SenderId, ReceiverId, Update),
                ?LOG_INFO("   📋 Chat list edit update sent for message ~p", [MessageId]);
            
            {ok, false} ->
                %% Não é a última mensagem - não atualizar chat list
                ?LOG_INFO("   🚫 Message ~p is not the last message - skipping chat list update", [MessageId]);
            
            {error, Error} ->
                ?LOG_ERROR("   ❌ Error checking if message is last: ~p", [Error])
        end
    catch
        Class:Reason:Stacktrace ->
            io:format("❌ Exception in send_chat_list_edit_update: ~p:~p~nStacktrace: ~p", 
                      [Class, Reason, Stacktrace]),
            ?LOG_ERROR("   ❌ Failed to send chat list edit update")
    end.

%% Enviar atualização para chat list page
send_chat_list_update(SenderId, ReceiverId, Content, MessageId) ->
    try
        message_router:send_chat_list_update(SenderId, ReceiverId, Content, MessageId)
    catch
        _:_ -> ok
    end.

send_json(Req, Status, Map, State) ->
    Json = jsx:encode(Map),
    Req2 = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req),
    {ok, Req2, State}.