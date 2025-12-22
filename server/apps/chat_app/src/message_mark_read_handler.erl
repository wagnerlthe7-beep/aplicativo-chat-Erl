-module(message_mark_read_handler).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> -> handle_mark_read(Req0, State);
        _ ->
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

handle_mark_read(Req0, State) ->
    Result =
        try
            UserId = cowboy_req:binding(user_id, Req0),
            ContactId = cowboy_req:binding(contact_id, Req0),

            ?LOG_INFO("📖 Marking messages as READ: user=~p contact=~p", [UserId, ContactId]),

            case mark_messages_read(UserId, ContactId) of
                ok ->
                    send_json(Req0, 200, #{status => <<"ok">>});
                {error, Reason2} ->
                    ?LOG_ERROR("❌ Erro ao marcar mensagens como lidas: ~p", [Reason2]),
                    send_json(Req0, 500, #{error => <<"internal_error">>})
            end
        catch
            Class:Reason3 ->
                ?LOG_ERROR("❌ Exceção em handle_mark_read: ~p:~p", [Class, Reason3]),
                send_json(Req0, 500, #{error => <<"server_error">>})
        end,
    {ok, Result, State}.

%% Marca todas as mensagens de ContactId -> UserId como "read"
mark_messages_read(UserIdBin, ContactIdBin) ->
    try
        UserId = binary_to_integer(UserIdBin),
        ContactId = binary_to_integer(ContactIdBin),

        db_pool:with_connection(fun(Conn) ->
            %% 1) Buscar mensagens ainda não lidas (sent/delivered)
            SqlSelect = "
                SELECT id
                FROM messages
                WHERE sender_id = $1
                  AND receiver_id = $2
                  AND status IN ('sent','delivered')
            ",
            case epgsql:equery(Conn, SqlSelect, [ContactId, UserId]) of
                {ok, _, []} -> ok;
                {ok, _, Rows} ->
                    MessageIds = [Id || {Id} <- Rows],

                    %% 2) Atualizar para 'read'
                    SqlUpdate = "
                        UPDATE messages
                        SET status = 'read'
                        WHERE sender_id = $1
                          AND receiver_id = $2
                          AND status IN ('sent','delivered')
                    ",
                    _ = epgsql:equery(Conn, SqlUpdate, [ContactId, UserId]),

                    %% 3) Enviar eventos 'message_read' para o remetente, se online
                    lists:foreach(
                      fun(MsgId) ->
                          message_router:mark_message_read(
                            list_to_binary(integer_to_list(MsgId)),
                            UserIdBin
                          )
                      end,
                      MessageIds
                    ),
                    ok;
                {error, Error} ->
                    {error, Error}
            end
        end)
    catch
        Class:Reason ->
            ?LOG_ERROR("❌ Exceção em mark_messages_read: ~p:~p", [Class, Reason]),
            {error, {Class, Reason}}
    end.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).


