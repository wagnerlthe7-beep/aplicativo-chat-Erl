%%%-------------------------------------------------------------------
%%% message_ack_handler.erl - Handler HTTP para receber ACKs de mensagens
%%% 
%%% Este handler é CRÍTICO para o sistema de entrega estilo WhatsApp:
%%% - Recebe ACK do cliente quando mensagem é recebida
%%% - Marca a mensagem como "delivered" no banco
%%% - Notifica o remetente sobre o status de entrega
%%%
%%% Endpoint:
%%%   POST /api/messages/ack - Receber ACK de mensagem
%%%-------------------------------------------------------------------
-module(message_ack_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    
    io:format("📨 [Message ACK Handler] ~s~n", [Method]),
    
    case Method of
        <<"POST">> ->
            handle_ack(Req0, State);
        _ ->
            Req1 = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"method_not_allowed">>}), Req0),
            {ok, Req1, State}
    end.

%% Processar ACK de mensagem
handle_ack(Req0, State) ->
    %% Verificar autenticação
    case get_user_from_token(Req0) of
        {ok, UserId} ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            
            case jsx:decode(Body, [return_maps]) of
                #{<<"message_id">> := MessageId, <<"status">> := Status} ->
                    io:format("📨 [ACK] Recebido de user ~p para mensagem ~p (status: ~s)~n", 
                              [UserId, MessageId, Status]),
                    
                    case process_ack(UserId, MessageId, Status) of
                        ok ->
                            io:format("✅ [ACK] Processado com sucesso~n"),
                            Req2 = cowboy_req:reply(200, #{
                                <<"content-type">> => <<"application/json">>
                            }, jsx:encode(#{success => true}), Req1),
                            {ok, Req2, State};
                        {error, Reason} ->
                            io:format("❌ [ACK] Erro ao processar: ~p~n", [Reason]),
                            Req2 = cowboy_req:reply(500, #{
                                <<"content-type">> => <<"application/json">>
                            }, jsx:encode(#{error => <<"processing_failed">>}), Req1),
                            {ok, Req2, State}
                    end;
                _ ->
                    Req2 = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => <<"message_id_and_status_required">>}), Req1),
                    {ok, Req2, State}
            end;
        {error, Reason} ->
            io:format("❌ [ACK] Auth failed: ~p~n", [Reason]),
            Req1 = cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"unauthorized">>}), Req0),
            {ok, Req1, State}
    end.

%% Processar o ACK
process_ack(UserId, MessageId, <<"delivered">>) ->
    %% Converter MessageId para integer (vem como binary do JSON)
    MessageIdInt = binary_to_integer_safe(MessageId),
    
    %% 1. Marcar mensagem como delivered no banco
    case message_repo:mark_message_delivered(MessageIdInt) of
        Result when Result =:= ok; element(1, Result) =:= ok ->
            %% 2. Obter informações da mensagem para notificar o remetente
            case message_repo:get_message_sender(MessageIdInt) of
                {ok, SenderId} ->
                    %% 3. Notificar o remetente via WebSocket (se online)
                    notify_sender_delivered(SenderId, MessageId, UserId),
                    ok;
                {error, _} ->
                    %% Mensagem marcada como delivered, mas não conseguiu notificar
                    %% Isso não é um erro crítico
                    ok
            end;
        {error, Reason} ->
            {error, Reason}
    end;

process_ack(UserId, MessageId, <<"read">>) ->
    %% Converter MessageId para integer (vem como binary do JSON)
    MessageIdInt = binary_to_integer_safe(MessageId),
    
    %% 1. Marcar mensagem como read no banco
    case message_repo:mark_message_read(MessageIdInt, UserId) of
        Result when Result =:= ok; element(1, Result) =:= ok ->
            %% 2. Obter informações da mensagem para notificar o remetente
            case message_repo:get_message_sender(MessageIdInt) of
                {ok, SenderId} ->
                    %% 3. Notificar o remetente via WebSocket (se online)
                    notify_sender_read(SenderId, MessageId, UserId),
                    ok;
                {error, _} ->
                    ok
            end;
        {error, Reason} ->
            {error, Reason}
    end;

process_ack(_, _, _) ->
    {error, invalid_status}.

%% Notificar remetente que mensagem foi entregue
notify_sender_delivered(SenderId, MessageId, ReceiverUserId) ->
    SenderIdBin = integer_to_binary_safe(SenderId),
    
    DeliveryMsg = #{
        <<"type">> => <<"message_delivered">>,
        <<"message_id">> => MessageId,
        <<"db_message_id">> => MessageId,
        <<"by_user">> => ReceiverUserId,
        <<"status">> => <<"delivered">>,
        <<"delivered_at">> => erlang:system_time(second)
    },
    
    %% Tentar enviar via WebSocket
    case user_session:is_websocket_alive(SenderIdBin) of
        true ->
            user_session:send_message(ReceiverUserId, SenderIdBin, DeliveryMsg),
            io:format("📡 [ACK] Notificação de delivered enviada para sender ~p~n", [SenderIdBin]);
        false ->
            io:format("⚠️ [ACK] Sender ~p não está online, notificação não enviada~n", [SenderIdBin])
    end.

%% Notificar remetente que mensagem foi lida
notify_sender_read(SenderId, MessageId, ReceiverUserId) ->
    SenderIdBin = integer_to_binary_safe(SenderId),
    
    ReadMsg = #{
        <<"type">> => <<"message_read">>,
        <<"message_id">> => MessageId,
        <<"db_message_id">> => MessageId,
        <<"by_user">> => ReceiverUserId,
        <<"status">> => <<"read">>,
        <<"read_at">> => erlang:system_time(second)
    },
    
    %% Tentar enviar via WebSocket
    case user_session:is_websocket_alive(SenderIdBin) of
        true ->
            user_session:send_message(ReceiverUserId, SenderIdBin, ReadMsg),
            io:format("📡 [ACK] Notificação de read enviada para sender ~p~n", [SenderIdBin]);
        false ->
            io:format("⚠️ [ACK] Sender ~p não está online, notificação não enviada~n", [SenderIdBin])
    end.

%%%-------------------------------------------------------------------
%%% Funções auxiliares
%%%-------------------------------------------------------------------

%% Obter user_id do token JWT
get_user_from_token(Req) ->
    case cowboy_req:header(<<"authorization">>, Req) of
        <<"Bearer ", Token/binary>> ->
            case auth_util:decode_jwt(Token) of
                {ok, Claims} ->
                    UserId = maps:get(<<"user_id">>, Claims),
                    {ok, UserId};
                {error, Reason} ->
                    {error, Reason}
            end;
        _ ->
            {error, no_token}
    end.

%% Converter integer para binary com segurança
integer_to_binary_safe(Int) when is_integer(Int) ->
    integer_to_binary(Int);
integer_to_binary_safe(Bin) when is_binary(Bin) ->
    Bin.

%% Converter binary para integer com segurança
binary_to_integer_safe(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin);
binary_to_integer_safe(Int) when is_integer(Int) ->
    Int.

