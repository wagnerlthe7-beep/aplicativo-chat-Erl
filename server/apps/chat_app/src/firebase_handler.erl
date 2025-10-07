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
                            io:format("📌 Phone: ~p, FirebaseUid: ~p~n", [Phone, FirebaseUid]),

                            %% 2) get_or_create_user (catch)
                            ResUser = catch auth_util:get_or_create_user(Phone, FirebaseUid),
                            io:format("🔹 get_or_create_user returned: ~p~n", [ResUser]),

                            case ResUser of
                                {ok, UserMap} ->
                                    %% 3) create_session_token (catch)
                                    ResToken = catch auth_util:create_session_token(UserMap),
                                    io:format("🔹 create_session_token returned: ~p~n", [ResToken]),

                                    case ResToken of
                                        {ok, Token} ->
                                            Response = #{token => Token, user => user_map_to_list(UserMap)},
                                            ReqF = reply_json(Req1, 200, Response),
                                            {ok, ReqF, State};
                                        {error, Reason} ->
                                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("create_token_error:~p",[Reason]))}),
                                            {ok, ReqF, State};
                                        {'EXIT', Reason} ->
                                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("create_token_exit:~p",[Reason]))}),
                                            {ok, ReqF, State};
                                        Other ->
                                            ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("create_token_unexpected:~p",[Other]))}),
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
