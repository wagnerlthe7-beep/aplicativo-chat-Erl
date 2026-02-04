%%%-------------------------------------------------------------------
%%% validate_session_handler.erl - VERSÃO CORRIGIDA
%%%-------------------------------------------------------------------
-module(validate_session_handler).
-export([init/2]).
-include_lib("kernel/include/logger.hrl").
init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            
            %% ✅✅✅ CORREÇÃO: try/catch CORRETO
            try 
                Data = jsx:decode(Body, [return_maps]),
                case maps:get(<<"access_token">>, Data, undefined) of
                    undefined ->
                        send_json(Req1, 400, #{error => <<"missing_access_token">>});
                    AccessToken ->
                        case auth_util:decode_jwt(AccessToken) of
                            {ok, Claims} ->
                                UserId = maps:get(<<"user_id">>, Claims),
                                SessionId = maps:get(<<"session_id">>, Claims, undefined),
                                
                                case check_session_valid(UserId, SessionId) of
                                    {ok, valid} ->
                                        send_json(Req1, 200, #{status => <<"valid">>});
                                    {error, revoked} ->
                                        send_json(Req1, 401, #{error => <<"session_revoked">>});
                                    {error, not_found} ->
                                        send_json(Req1, 401, #{error => <<"session_not_found">>});
                                    {error, _} ->
                                        send_json(Req1, 500, #{error => <<"internal_error">>})
                                end;
                            {error, _Reason} ->
                                send_json(Req1, 401, #{error => <<"invalid_token">>})
                        end
                end
            catch
                _:Reason ->
                    ?LOG_ERROR("❌ Error in validate_session: ~p", [Reason]),
                    send_json(Req1, 400, #{error => <<"invalid_request">>})
            end,
            {ok, Req1, State};
        _ ->
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

    check_session_valid(UserId, SessionId) ->
        try
            db_util:with_connection(fun(Conn) ->
                %% ✅✅✅ CORREÇÃO: Conversão manual sem safe_to_integer
                UserIdInt = case UserId of
                    U when is_binary(U) -> 
                        try binary_to_integer(U) 
                        catch _:_ -> throw({invalid_user_id, U}) 
                        end;
                    U when is_list(U) -> 
                        try list_to_integer(U) 
                        catch _:_ -> throw({invalid_user_id, U}) 
                        end;
                    U when is_integer(U) -> U;
                    _ -> throw({invalid_user_id, UserId})
                end,
                
                SessionIdInt = case SessionId of
                    S when is_binary(S) -> 
                        try binary_to_integer(S) 
                        catch _:_ -> throw({invalid_session_id, S}) 
                        end;
                    S when is_list(S) -> 
                        try list_to_integer(S) 
                        catch _:_ -> throw({invalid_session_id, S}) 
                        end;
                    S when is_integer(S) -> S;
                    _ -> throw({invalid_session_id, SessionId})
                end,
                
                Sql = "SELECT revoked FROM sessions WHERE user_id = $1 AND id = $2 AND revoked = false",  %% ✅✅✅ SÓ verifica revoked
                case epgsql:equery(Conn, Sql, [UserIdInt, SessionIdInt]) of
                    {ok, _, [{_}]} ->  %% ✅✅✅ Se encontrou = VÁLIDA (não importa expires_at)
                        {ok, valid};
                    {ok, _, []} ->
                        {error, not_found};  %% ❌ Sessão revogada ou não existe
                    {error, Reason} ->
                        {error, Reason}
                end
            end)
        catch
            throw:{invalid_user_id, _} -> {error, invalid_user_id};
            throw:{invalid_session_id, _} -> {error, invalid_session_id};
            _:_ -> {error, internal_error}
        end.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).