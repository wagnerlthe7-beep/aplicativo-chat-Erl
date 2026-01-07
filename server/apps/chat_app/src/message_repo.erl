%% message_repo.erl - módulo correto
-module(message_repo).

-include_lib("kernel/include/logger.hrl").

-export([
    save_message/3, 
    get_user_messages/1, 
    mark_message_delivered/1, 
    mark_messages_as_delivered/1, 
    get_undelivered_messages/1, 
    mark_message_read/1, 
    get_chat_history/2, 
    get_chat_history/4, 
    get_message_sender/1,
    
    %% Novas funções para operações avançadas
    get_message_details/1,
    get_message_replies/1,
    get_edit_history/1,
    can_edit_message/2,
    can_delete_message/2,
    binary_to_integer_wrapper/1,
    get_user_name/1,
    save_reply_message/4
]).

save_message(SenderId, ReceiverId, Content) ->
    db_pool:with_connection(fun(Conn) ->
        SenderIdInt = erlang:binary_to_integer(SenderId),
        ReceiverIdInt = erlang:binary_to_integer(ReceiverId),
        Sql = "INSERT INTO messages (sender_id, receiver_id, content, message_type, status, sent_at) \n               VALUES ($1, $2, $3, 'text', 'sent', NOW()) RETURNING id",
        case epgsql:equery(Conn, Sql, [SenderIdInt, ReceiverIdInt, Content]) of
            {ok, _, _, [{MessageId}]} -> {ok, MessageId};
            {error, Error} -> {error, Error}
        end
    end).

get_user_messages(UserId) ->
    db_pool:with_connection(fun(Conn) ->
        UserIdInt = erlang:binary_to_integer(UserId),
        Sql = "SELECT m.id, m.sender_id, m.content, m.sent_at, m.status,\n                      u.name as sender_name\n               FROM messages m \n               JOIN users u ON m.sender_id = u.id \n               WHERE m.receiver_id = $1 \n               ORDER BY m.sent_at DESC \n               LIMIT 50",
        case epgsql:equery(Conn, Sql, [UserIdInt]) of
            {ok, _, Rows} -> {ok, Rows};
            {error, Error} -> {error, Error}
        end
    end).

mark_message_delivered(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages SET status = 'delivered' WHERE id = $1",
        epgsql:equery(Conn, Sql, [MessageId])
    end).

mark_messages_as_delivered([]) -> ok;
mark_messages_as_delivered(MessageIds) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages SET status = 'delivered' WHERE id = ANY($1)",
        epgsql:equery(Conn, Sql, [MessageIds])
    end).

get_undelivered_messages(UserId) ->
    db_pool:with_connection(fun(Conn) ->
        UserIdInt = binary_to_integer_wrapper(UserId),
        Sql = "
            SELECT m.id,
                   m.sender_id,
                   m.receiver_id,
                   m.content,
                   m.sent_at,
                   m.status,
                   m.reply_to_id,
                   m_original.content as reply_to_text,
                   m_original.sender_id as reply_to_sender_id,
                   u1.name as reply_to_sender_name
            FROM messages m
            LEFT JOIN messages m_original ON m.reply_to_id = m_original.id
            LEFT JOIN users u1 ON m_original.sender_id = u1.id
            WHERE m.receiver_id = $1
              AND m.status = 'sent'
            ORDER BY m.sent_at ASC",
        case epgsql:equery(Conn, Sql, [UserIdInt]) of
            {ok, _, Rows} ->
                Messages = lists:map(fun({
                    Id, SenderId, ReceiverId, Content, SentAt, Status,
                    ReplyToId, ReplyToText, ReplyToSenderId, ReplyToSenderName
                }) ->
                    #{
                        <<"id">> => Id,
                        <<"sender_id">> => SenderId,
                        <<"receiver_id">> => ReceiverId,
                        <<"content">> => Content,
                        <<"sent_at">> => SentAt,
                        <<"status">> => Status,
                        <<"reply_to_id">> => ReplyToId,
                        <<"reply_to_text">> => ReplyToText,
                        <<"reply_to_sender_id">> => ReplyToSenderId,
                        <<"reply_to_sender_name">> => ReplyToSenderName
                    }
                end, Rows),
                {ok, Messages};
            {error, Error} -> {error, Error}
        end
    end).

mark_message_read(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages SET status = 'read' WHERE id = $1",
        epgsql:equery(Conn, Sql, [MessageId])
    end).

get_message_sender(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "SELECT sender_id FROM messages WHERE id = $1",
        case epgsql:equery(Conn, Sql, [MessageId]) of
            {ok, _, [{SenderId}]} -> {ok, integer_to_binary(SenderId)};
            {ok, _, []} -> {error, not_found};
            {error, Error} -> {error, Error}
        end
    end).

get_chat_history(UserId, ContactId) ->
    get_chat_history(UserId, ContactId, 100, 0).

get_chat_history(UserId, ContactId, Limit, Offset) ->
    try
        UserIdInt = binary_to_integer_wrapper(UserId),
        ContactIdInt = binary_to_integer_wrapper(ContactId),
        LimitInt0 = case Limit of
            _ when is_integer(Limit) -> Limit;
            _ when is_binary(Limit) -> binary_to_integer_wrapper(Limit);
            _ -> 100
        end,
        LimitInt = case LimitInt0 < 1 of
            true -> 1;
            false -> case LimitInt0 > 2000 of true -> 2000; false -> LimitInt0 end
        end,
        OffsetInt0 = case Offset of
            _ when is_integer(Offset) -> Offset;
            _ when is_binary(Offset) -> binary_to_integer_wrapper(Offset);
            _ -> 0
        end,
        OffsetInt = case OffsetInt0 < 0 of true -> 0; false -> OffsetInt0 end,

        db_pool:with_connection(fun(Conn) ->
            Query = "\n                SELECT m.id, m.sender_id, m.receiver_id, m.content, m.sent_at, m.status,\n                       m.reply_to_id, \n                       CASE \n                           WHEN m_original.sender_id = $1 THEN 'Eu'\n                           ELSE u1.name \n                       END as reply_to_sender_name,\n                       m_original.sender_id as reply_to_sender_id,\n                       m_original.content as reply_to_text\n                FROM messages m\n                LEFT JOIN messages m_original ON m.reply_to_id = m_original.id\n                LEFT JOIN users u1 ON m_original.sender_id = u1.id\n                WHERE (m.sender_id = $1 AND m.receiver_id = $2)\n                   OR (m.sender_id = $2 AND m.receiver_id = $1)\n                ORDER BY m.sent_at DESC\n                LIMIT $3 OFFSET $4\n            ",
            case epgsql:equery(Conn, Query, [UserIdInt, ContactIdInt, LimitInt, OffsetInt]) of
                {ok, _, Rows} ->
                    Messages = lists:map(fun({Id, SenderId, ReceiverId, Content, SentAt, Status, ReplyToId, ReplyToSenderName, ReplyToSenderId, ReplyToText}) ->
                        #{
                            <<"id">> => Id,
                            <<"sender_id">> => SenderId,
                            <<"receiver_id">> => ReceiverId,
                            <<"content">> => Content,
                            <<"sent_at">> => list_to_binary(io_lib:format("~p", [SentAt])),
                            <<"status">> => Status,
                            <<"type">> => <<"message">>,
                            %% ✅ CAMPOS DE RESPOSTA
                            <<"reply_to_id">> => case ReplyToId of null -> null; _ -> ReplyToId end,
                            <<"reply_to_sender_name">> => case ReplyToSenderName of null -> null; _ -> ReplyToSenderName end,
                            <<"reply_to_sender_id">> => case ReplyToSenderId of null -> null; _ -> ReplyToSenderId end,
                            <<"reply_to_text">> => case ReplyToText of null -> null; _ -> ReplyToText end
                        }
                    end, Rows),
                    {ok, Messages};
                {error, Error} -> {error, Error}
            end
        end)
    catch
        Error:Reason ->
            io:format("❌ Erro: ~p:~p~n", [Error, Reason]),
            {error, conversion_error}
    end.

%%%===================================================================
%%% Funções para operações avançadas de mensagens
%%%===================================================================

%% @doc Buscar mensagem com detalhes completos
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

%% @doc Buscar respostas de uma mensagem
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

%% @doc Buscar histórico de edições
get_edit_history(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            MsgIdInt = binary_to_integer_wrapper(MessageId),
            
            Sql = "SELECT h.id, h.original_content, h.edited_content, 
                          h.edited_by, u.name as editor_name, h.edited_at
                   FROM message_edit_history h
                   JOIN users u ON h.edited_by = u.id
                   WHERE h.message_id = $1
                   ORDER BY h.edited_at DESC",
            
            case epgsql:equery(Conn, Sql, [MsgIdInt]) of
                {ok, _, Rows} ->
                    History = lists:map(fun(Row) ->
                        {Id, Original, Edited, EditorId, EditorName, EditedAt} = Row,
                        #{
                            id => Id,
                            original_content => Original,
                            edited_content => Edited,
                            edited_by => EditorId,
                            editor_name => EditorName,
                            edited_at => EditedAt
                        }
                    end, Rows),
                    {ok, History};
                {error, Error} ->
                    {error, Error}
            end
        catch
            _:_ -> {error, invalid_id}
        end
    end).

%% @doc Verificar se usuário pode editar mensagem
can_edit_message(MessageId, UserId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            MsgIdInt = binary_to_integer_wrapper(MessageId),
            UserIdInt = binary_to_integer_wrapper(UserId),
            
            Sql = "SELECT m.id, m.sender_id, m.sent_at, m.is_deleted
                   FROM messages m
                   WHERE m.id = $1 AND m.sender_id = $2",
            
            case epgsql:equery(Conn, Sql, [MsgIdInt, UserIdInt]) of
                {ok, _, [{_, _, SentAt, IsDeleted}]} ->
                    % Verificar tempo (15 minutos)
                    Now = erlang:system_time(second),
                    SentAtUnix = case SentAt of
                        {Date, Time} ->
                            calendar:datetime_to_gregorian_seconds({Date, Time});
                        _ -> Now
                    end,
                    
                    TimeDiff = Now - SentAtUnix,
                    MaxEditTime = 15 * 60, % 15 minutos em segundos
                    
                    if
                        IsDeleted -> {error, already_deleted};
                        TimeDiff > MaxEditTime -> {error, edit_time_expired};
                        true -> {ok, can_edit}
                    end;
                {ok, _, []} ->
                    {error, not_authorized};
                _ ->
                    {error, not_found}
            end
        catch
            _:_ -> {error, invalid_id}
        end
    end).

%% @doc Verificar se usuário pode deletar mensagem
can_delete_message(MessageId, UserId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            MsgIdInt = binary_to_integer_wrapper(MessageId),
            UserIdInt = binary_to_integer_wrapper(UserId),
            
            Sql = "SELECT m.sender_id, m.receiver_id, m.sent_at, m.is_deleted
                   FROM messages m
                   WHERE m.id = $1",
            
            case epgsql:equery(Conn, Sql, [MsgIdInt]) of
                {ok, _, [{SenderId, ReceiverId, SentAt, IsDeleted}]} ->
                    if
                        IsDeleted -> {error, already_deleted};
                        SenderId =:= UserIdInt ->
                            % Remetente pode deletar até 1 hora
                            Now = erlang:system_time(second),
                            SentAtUnix = case SentAt of
                                {Date, Time} ->
                                    calendar:datetime_to_gregorian_seconds({Date, Time});
                                _ -> Now
                            end,
                            
                            TimeDiff = Now - SentAtUnix,
                            MaxDeleteTime = 3600, % 1 hora em segundos
                            
                            if
                                TimeDiff > MaxDeleteTime -> {error, delete_time_expired};
                                true -> {ok, can_delete}
                            end;
                        ReceiverId =:= UserIdInt ->
                            % Destinatário sempre pode deletar
                            {ok, can_delete};
                        true ->
                            {error, not_authorized}
                    end;
                {ok, _, []} ->
                    {error, not_found};
                _ ->
                    {error, db_error}
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

%% @doc Buscar nome do usuário por ID
get_user_name(UserId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            UserIdInt = binary_to_integer_wrapper(UserId),
            
            Sql = "SELECT name FROM users WHERE id = $1",
            
            case epgsql:equery(Conn, Sql, [UserIdInt]) of
                {ok, _, [{Name}]} -> {ok, Name};
                {ok, _, []} -> {error, not_found};
                {error, Error} -> {error, Error}
            end
        catch
            _:_ -> {error, invalid_id}
        end
    end).

%% @doc Salvar mensagem de resposta com reply_to_id
save_reply_message(SenderId, ReceiverId, Content, ReplyToId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            SenderIdInt = binary_to_integer_wrapper(SenderId),
            ReceiverIdInt = binary_to_integer_wrapper(ReceiverId),
            ReplyToInt = binary_to_integer_wrapper(ReplyToId),
            
            Sql = "INSERT INTO messages (sender_id, receiver_id, content, message_type, status, sent_at, reply_to_id) 
                    VALUES ($1, $2, $3, 'text', 'sent', NOW(), $4) RETURNING id",
            
            case epgsql:equery(Conn, Sql, [SenderIdInt, ReceiverIdInt, Content, ReplyToInt]) of
                {ok, _, _, [{MessageId}]} -> {ok, MessageId};
                {error, Error} -> 
                    ?LOG_ERROR("❌ Error saving reply message: ~p", [Error]),
                    {error, Error}
            end
        catch
            Exception:Reason ->
                ?LOG_ERROR("❌ Exception in save_reply_message: ~p", [Exception, Reason]),
                {error, invalid_parameters}
        end
    end).
