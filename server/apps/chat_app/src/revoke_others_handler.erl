%%%-------------------------------------------------------------------
%%% revoke_others_handler.erl - CORRIGIDO
%%% Handler para /auth/revoke-others (revoga todas as outras sessões)
%%%-------------------------------------------------------------------
-module(revoke_others_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            ?LOG_INFO("🚫 /auth/revoke-others Body: ~p", [Body]),
            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"access_token">> := AccessToken, <<"device_uuid">> := _DeviceUUID} ->
                    ?LOG_INFO("🔍 Received access token: ~s", [AccessToken]),
                    %% Decodificar JWT para obter user_id
                    case auth_util:decode_jwt(AccessToken) of
                        {ok, Claims} ->
                            ?LOG_INFO("✅ JWT decoded successfully: ~p", [Claims]),
                            UserId = maps:get(<<"user_id">>, Claims),
                            
                            %% ✅✅✅ CORREÇÃO: Tratar session_id opcional
                            SessionId = case maps:get(<<"session_id">>, Claims, undefined) of
                                undefined -> 
                                    ?LOG_WARNING("⚠️ No session_id in JWT, using user_id as fallback"),
                                    UserId;
                                Sid -> Sid
                            end,
                            
                            ?LOG_INFO("🚫 Revoking other sessions for user ~p, keeping session ~p", [UserId, SessionId]),
                            
                            case auth_util:revoke_other_sessions(UserId, SessionId) of
                                ok ->
                                    ?LOG_INFO("✅ Successfully revoked other sessions for user ~p", [UserId]),
                                    ReqF = reply_json(Req1, 200, #{ok => <<"other_sessions_revoked">>}),
                                    {ok, ReqF, State};
                                {error, Reason} ->
                                    ?LOG_ERROR("❌ Failed to revoke other sessions: ~p", [Reason]),
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("revoke_failed:~p",[Reason]))}),
                                    {ok, ReqF, State}
                            end;
                        {error, Reason} ->
                            ?LOG_ERROR("❌ Failed to decode JWT: ~p", [Reason]),
                            ReqF = reply_json(Req1, 401, #{error => list_to_binary(io_lib:format("jwt_decode_error:~p",[Reason]))}),
                            {ok, ReqF, State}
                    end;
                _ ->
                    ReqF = reply_json(Req1, 400, #{error => <<"invalid_payload">>}),
                    {ok, ReqF, State}
            end;
        _ ->
            ReqF = reply_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, ReqF, State}
    end.

%%%-------------------------------------------------------------------
%%% Helper functions - COPIADAS do firebase_handler
%%%-------------------------------------------------------------------
reply_json(Req, Status, Map) when is_map(Map) ->
    %% converte para lista de pares {binary_key, safe_value}
    SafeList = lists:map(fun({K,V}) -> {binaryize(K), safe_value(V)} end, maps:to_list(Map)),
    Body = jsx:encode(SafeList),
    ReplyRes = cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Body, Req),
    case ReplyRes of
        {ok, Req2} -> Req2;
        Req2 -> Req2
    end.

%% normaliza valores para JSON (binaries / proplist)
safe_value(undefined) ->
    null;
safe_value(null) ->
    null;
safe_value(true) ->
    true;
safe_value(false) ->
    false;
safe_value(Value) when is_binary(Value) ->
    Value;
safe_value(Value) when is_list(Value) ->
    %% Pode ser string ou lista de tuplas: tratamos apenas strings aqui
    case io_lib:char_list(Value) of
        true -> list_to_binary(Value);
        false -> Value
    end;
safe_value(Value) when is_integer(Value); is_float(Value) ->
    Value;
safe_value(Value) when is_map(Value) ->
    Value;
safe_value(Other) ->
    %% Último fallback: apenas retorna sem tentar converter
    Other.

binaryize(Key) when is_atom(Key) -> list_to_binary(atom_to_list(Key));
binaryize(Key) when is_binary(Key) -> Key.