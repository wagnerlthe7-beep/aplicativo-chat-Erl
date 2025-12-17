%%%-------------------------------------------------------------------
%%% message_history_handler.erl - VERSÃO CORRIGIDA
%%%-------------------------------------------------------------------
-module(message_history_handler).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> -> get_chat_history(Req0, State);
        _ -> 
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

get_chat_history(Req0, State) ->
    try
        UserId = cowboy_req:binding(user_id, Req0),
        ContactId = cowboy_req:binding(contact_id, Req0),
            
        ?LOG_INFO("📨 Buscando histórico: user ~p -> contact ~p", [UserId, ContactId]),
            
        case message_repo:get_chat_history(UserId, ContactId) of
            {ok, Messages} ->
                Response = #{<<"messages">> => Messages},
                send_json(Req0, 200, Response);
            {error, invalid_user_id} ->
                send_json(Req0, 400, #{error => <<"invalid_user_id">>});
            {error, Reason} ->
                ?LOG_ERROR("❌ Erro ao buscar histórico: ~p", [Reason]),
                send_json(Req0, 500, #{error => <<"internal_error">>})
        end
    catch
        _:Error ->
            ?LOG_ERROR("❌ Erro em get_chat_history: ~p", [Error]),
            send_json(Req0, 500, #{error => <<"server_error">>})
    end,
    {ok, Req0, State}.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).
