%%%-------------------------------------------------------------------
%%% Handler HTTP para /auth/check-user - Verifica se usuário é novo
%%%-------------------------------------------------------------------
-module(check_user_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    io:format("🔍 Método recebido em check_user: ~p~n", [Method]),

    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            io:format("📥 Body recebido em check_user: ~p~n", [Body]),

            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"idToken">> := IdToken} ->
                    io:format("🟢 idToken recebido em check_user: ~p~n", [IdToken]),

                    %% 1) verifica token Firebase
                    ResVerify = catch auth_util:verify_firebase_token(IdToken),
                    io:format("🔎 verify_firebase_token result em check_user: ~p~n", [ResVerify]),

                    case ResVerify of
                        {ok, Claims} ->
                            Phone = maps:get(<<"phoneNumber">>, Claims, undefined),
                            io:format("📌 Phone em check_user: ~p~n", [Phone]),

                            %% 2) verificar se usuário existe
                            ResCheck = catch check_user_exists(Phone),
                            io:format("🔹 check_user_exists returned: ~p~n", [ResCheck]),

                            case ResCheck of
                                {ok, IsNew} ->
                                    Response = #{
                                        is_new_user => IsNew
                                    },
                                    ReqF = reply_json(Req1, 200, Response),
                                    {ok, ReqF, State};
                                {error, Reason} ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("check_error:~p",[Reason]))}),
                                    {ok, ReqF, State};
                                {'EXIT', Reason} ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("check_exit:~p",[Reason]))}),
                                    {ok, ReqF, State};
                                Other ->
                                    ReqF = reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("check_unexpected:~p",[Other]))}),
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
%%% Função auxiliar para verificar se usuário existe
%%%-------------------------------------------------------------------
check_user_exists(Phone) ->
    ?LOG_INFO("🔍 Checking if user exists for phone: ~s", [Phone]),
    db_util:with_connection(fun(Conn) ->
        PhoneBin = auth_util:ensure_binary_utf8(Phone),
        Sql = "SELECT COUNT(*) FROM users WHERE phone = $1",
        case epgsql:equery(Conn, Sql, [PhoneBin]) of
            {ok, _, [{Count}]} ->
                IsNew = (Count =:= 0),
                ?LOG_INFO("✅ User exists check: ~p (is_new: ~p)", [Count, IsNew]),
                {ok, IsNew};
            Err -> 
                ?LOG_ERROR("❌ Failed to check user existence: ~p", [Err]),
                {error, {db_error, Err}}
        end
    end).

%%%-------------------------------------------------------------------
%%% Helpers (copiados do firebase_handler)
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
