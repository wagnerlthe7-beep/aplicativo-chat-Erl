%%%-------------------------------------------------------------------
%%% Handler HTTP para /auth/firebase usando cowboy_handler
%%%-------------------------------------------------------------------
-module(firebase_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("🔥 Método recebido: ~p~n", [Method]),

    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            io:format("📥 Body recebido: ~p~n", [Body]),

            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"idToken">> := IdToken} ->
                    io:format("🟢 idToken recebido: ~p~n", [IdToken]),

                    %% 1) verifica token Firebase (catch para evitar crash)
                    ResVerify = catch auth_util:verify_firebase_token(IdToken),
                    io:format("🔎 verify_firebase_token result: ~p~n", [ResVerify]),

                    case ResVerify of
                        {ok, Claims} ->
                            Phone = maps:get(<<"phoneNumber">>, Claims, undefined),
                            FirebaseUid = maps:get(<<"localId">>, Claims, undefined),
                            UserName = maps:get(<<"user_name">>, Dec, undefined), %% ✅✅✅ NOVO: Nome do usuário
                            io:format("📌 Phone: ~p, FirebaseUid: ~p, UserName: ~p~n", [Phone, FirebaseUid, UserName]),

                            %% 2) get_or_create_user (catch) - com nome se fornecido
                            ResUser = catch auth_util:get_or_create_user(Phone, FirebaseUid, UserName),
                            io:format("🔹 get_or_create_user returned: ~p~n", [ResUser]),

                            case ResUser of
                                {ok, UserMap} ->
                                    %% 3) create_session_token (catch)
                                    DeviceUUID = maps:get(<<"device_uuid">>, Dec, <<"unknown_device">>),
                                    DeviceInfo = maps:get(<<"device_info">>, Dec, <<"unknown">>),

                                    ResSession = catch auth_util:create_session_for_user(maps:get(id, UserMap), DeviceUUID, DeviceInfo),
                                    io:format("🆕 create_session_for_user result: ~p~n", [ResSession]),

                                    case ResSession of
                                        {ok, SessData} ->
                                            SessionId = maps:get(session_id, SessData),
                                            RefreshToken = maps:get(refresh_token, SessData),
                                            
                                            %% ✅✅✅ CORREÇÃO: REVOGAR SESSÕES ANTIGAS AUTOMATICAMENTE
                                            UserId = maps:get(id, UserMap),
                                            case auth_util:revoke_other_sessions(UserId, SessionId) of
                                                ok -> 
                                                    ?LOG_INFO("✅ Sessões antigas revogadas para usuário ~p", [UserId]);
                                                {error, ReasonRevoke} ->
                                                    ?LOG_WARNING("⚠️ Não foi possível revogar sessões antigas: ~p", [ReasonRevoke])
                                            end,
                                            %% ✅✅✅ FIM DA CORREÇÃO
                                            
                                            %% ✅✅✅ CORREÇÃO: Garantir que session_id está no UserMap para o JWT
                                            UserMapWithSession = UserMap#{session_id => SessionId},
                                            ResAccess = catch auth_util:create_access_jwt(UserMapWithSession, SessionId),
                                            case ResAccess of
                                                {ok, AccessToken} ->
                                                    Response = #{
                                                        access_token => AccessToken,
                                                        refresh_token => RefreshToken,
                                                        user => user_map_to_list(UserMap)
                                                    },
                                                    ReqF = reply_json(Req1, 200, Response),
                                                    {ok, ReqF, State};
                                                {error, Reason2} ->
                                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("access_token_error:~p",[Reason2]))}),
                                                    {ok, ReqF, State}
                                            end;
                                        {error, ReasonSess} ->
                                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("create_session_error:~p",[ReasonSess]))}),
                                            {ok, ReqF, State};
                                        {'EXIT', ReasonSess} ->
                                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("session_exit:~p",[ReasonSess]))}),
                                            {ok, ReqF, State}
                                    end;
                                {error, Reason} ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("user_error:~p",[Reason]))}),
                                    {ok, ReqF, State};
                                {'EXIT', Reason} ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("user_exit:~p",[Reason]))}),
                                    {ok, ReqF, State};
                                Other ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("user_unexpected:~p",[Other]))}),
                                    {ok, ReqF, State}
                            end;

                        {error, Reason} ->
                            ReqF = reply_json(Req1, 401, #{error => list_to_binary(io_lib:format("verify_error:~p",[Reason]))}),
                            {ok, ReqF, State};
                        {'EXIT', Reason} ->
                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("verify_exit:~p",[Reason]))}),
                            {ok, ReqF, State};
                        Other ->
                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("verify_unexpected:~p",[Other]))}),
                            {ok, ReqF, State}
                    end;

                _ ->
                    ReqF = reply_json(Req1, 400, #{error => <<"invalid_payload">>}),
                    {ok, ReqF, State}
            end;

        _Other ->
            ReqF = reply_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, ReqF, State}
    end.

%%%-------------------------------------------------------------------
%%% Helpers
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


user_map_to_list(UserMap) when is_map(UserMap) ->
    lists:map(fun({K,V}) -> {binaryize(K), safe_value(V)} end, maps:to_list(UserMap)).

binaryize(Key) when is_atom(Key) -> list_to_binary(atom_to_list(Key));
binaryize(Key) when is_binary(Key) -> Key.
