%% message_repo.erl - módulo correto
-module(message_repo).
-export([save_message/3, get_user_messages/1, mark_message_delivered/1, mark_message_read/1, get_chat_history/2, get_chat_history/4]).

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

mark_message_read(MessageId) ->
    db_pool:with_connection(fun(Conn) ->
        Sql = "UPDATE messages SET status = 'read' WHERE id = $1",
        epgsql:equery(Conn, Sql, [MessageId])
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
            Query = "\n                SELECT id, sender_id, receiver_id, content, sent_at, status\n                FROM messages \n                WHERE (sender_id = $1 AND receiver_id = $2)\n                   OR (sender_id = $2 AND receiver_id = $1)\n                ORDER BY sent_at DESC\n                LIMIT $3 OFFSET $4\n            ",
            case epgsql:equery(Conn, Query, [UserIdInt, ContactIdInt, LimitInt, OffsetInt]) of
                {ok, _, Rows} ->
                    Messages = lists:map(fun({Id, SenderId, ReceiverId, Content, SentAt, Status}) ->
                        #{
                            <<"id">> => Id,
                            <<"sender_id">> => SenderId,
                            <<"receiver_id">> => ReceiverId,
                            <<"content">> => Content,
                            <<"sent_at">> => list_to_binary(io_lib:format("~p", [SentAt])),
                            <<"status">> => Status,
                            <<"type">> => <<"message">>
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

binary_to_integer_wrapper(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary));
binary_to_integer_wrapper(Integer) when is_integer(Integer) ->
    Integer.
