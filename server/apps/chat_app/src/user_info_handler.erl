%% user_info_handler.erl - NOVO ARQUIVO
-module(user_info_handler).
-export([init/2, get_user_from_db/1]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> -> get_user_info(Req0, State);
        _ -> 
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

get_user_info(Req0, State) ->
    try
        UserId = cowboy_req:binding(user_id, Req0),
        
        ?LOG_INFO("🔍 Buscando informações do usuário: ~p", [UserId]),
        
        case get_user_from_db(UserId) of
            {ok, UserInfo} ->
                send_json(Req0, 200, UserInfo);
            {error, not_found} ->
                send_json(Req0, 404, #{error => <<"user_not_found">>});
            {error, Reason} ->
                ?LOG_ERROR("❌ Erro ao buscar usuário: ~p", [Reason]),
                send_json(Req0, 500, #{error => <<"internal_error">>})
        end
    catch
        _:Error ->
            ?LOG_ERROR("❌ Erro em get_user_info: ~p", [Error]),
            send_json(Req0, 500, #{error => <<"server_error">>})
    end,
    {ok, Req0, State}.

get_user_from_db(UserId) ->
    db_pool:with_connection(fun(Conn) ->
        try
            UserIdInt = binary_to_integer_wrapper(UserId),
            
            Sql = "SELECT id, name, phone FROM users WHERE id = $1",
            ?LOG_INFO("📝 Executando query: ~p com params: ~p", [Sql, [UserIdInt]]),
            
            case epgsql:equery(Conn, Sql, [UserIdInt]) of
                {ok, _, [{Id, Name, Phone}]} ->
                    ?LOG_INFO("✅ Dados encontrados: ID=~p, Name=~p, Phone=~p", [Id, Name, Phone]),
                    UserInfo = #{
                        <<"id">> => erlang:integer_to_binary(Id),
                        <<"name">> => ensure_binary(Name),
                        <<"phone">> => ensure_binary(Phone)
                    },
                    {ok, UserInfo};
                {ok, _, []} ->
                    ?LOG_INFO("❌ Nenhum resultado para ID: ~p", [UserIdInt]),
                    {error, not_found};
                {error, Error} ->
                    ?LOG_ERROR("❌ Erro na query: ~p", [Error]),
                    {error, Error}
            end
        catch
            _:_ ->
                ?LOG_ERROR("❌ Erro na conversão do UserId: ~p", [UserId]),
                {error, invalid_user_id}
        end
    end).

binary_to_integer_wrapper(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary));
binary_to_integer_wrapper(Integer) when is_integer(Integer) ->
    Integer.

ensure_binary(Value) when is_binary(Value) -> Value;
ensure_binary(Value) when is_list(Value) -> list_to_binary(Value);
ensure_binary(Value) when is_integer(Value) -> erlang:integer_to_binary(Value);
ensure_binary(_Value) -> <<"">>.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).