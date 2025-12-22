%%%-------------------------------------------------------------------
%%% message_router.erl - VERSÃO CORRIGIDA
%%%-------------------------------------------------------------------

-module(message_router).

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
    get_offline_messages/1
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
            
            %% ✅ MENSAGEM PARA O DESTINATÁRIO (status = delivered se online)
            MessageToReceiver = #{<<"type">> => <<"message">>,
                                 <<"from">> => FromId,
                                 <<"to">> => ToId,
                                 <<"content">> => Content,
                                 <<"timestamp">> => Timestamp,
                                 <<"message_id">> => MessageId,
                                 <<"db_message_id">> => DbMessageId,
                                 <<"status">> => <<"delivered">>,  %% para o destinatário, já entregue
                                 <<"should_increase_unread">> => true},
            
            %% ✅ Enviar para destinatário (online -> delivered; offline -> continua sent)
            case user_session:send_message(FromId, ToId, MessageToReceiver) of
                ok ->
                    io:format("   ✅✅✅ Enviada para DESTINATÁRIO ~p~n", [ToId]),
                    
                    %% ✅ ATUALIZAR BD: status = 'delivered'
                    message_repo:mark_message_delivered(DbMessageId),
                    
                    %% ✅ Confirmação de entrega para o remetente (AGORA RETORNADA NA TUPLA)
                    %% O chamador (ws_handler) deve enviar a notificação imediatamente.
                    
                    io:format("   ✅ Mensagem entregue para ~p. Retornando status 'delivered' para remetente.~n", [ToId]),
                    
                    {ok, MessageToReceiver, delivered};
                    
                {error, user_offline} ->
                    io:format("   💾 Usuário ~p offline - armazenando mensagem (status=sent)~n", [ToId]),
                    store_offline_message(ToId, MessageToReceiver#{
                      <<"status">> => <<"sent">>
                    }),
                    {ok, MessageToReceiver, sent};
                    
                {error, Reason} ->
                    io:format("   ❌ Erro ao enviar mensagem: ~p~n", [Reason]),
                    {error, Reason}
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
    
    io:format("💾 Stored offline message for ~p: ~p~n", [UserId, MessageId]),
    ok.

%% @doc Limpa mensagens offline de um usuário
clear_offline_messages(UserId) ->
    F = fun() ->
        mnesia:delete({pending_messages, UserId})
    end,
    mnesia:transaction(F),
    io:format("🧹 Cleared offline messages for user: ~p~n", [UserId]).