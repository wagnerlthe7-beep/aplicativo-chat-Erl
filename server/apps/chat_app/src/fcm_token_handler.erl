%%%-------------------------------------------------------------------
%%% fcm_token_handler.erl - Handler HTTP para registar/remover FCM tokens
%%% 
%%% Endpoints:
%%%   POST /api/fcm/register   - Registar token FCM
%%%   POST /api/fcm/unregister - Remover token FCM
%%%-------------------------------------------------------------------
-module(fcm_token_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    Path = cowboy_req:path(Req0),
    
    io:format("📱 [FCM Token Handler] ~s ~s~n", [Method, Path]),
    
    %% Verificar autenticação
    case get_user_from_token(Req0) of
        {ok, UserId} ->
            handle_request(Method, Path, UserId, Req0, State);
        {error, Reason} ->
            io:format("❌ [FCM] Auth failed: ~p~n", [Reason]),
            Req1 = cowboy_req:reply(401, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"unauthorized">>}), Req0),
            {ok, Req1, State}
    end.

%% POST /api/fcm/register
handle_request(<<"POST">>, <<"/api/fcm/register">>, UserId, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"fcm_token">> := FcmToken} = Data ->
            DeviceType = maps:get(<<"device_type">>, Data, <<"android">>),
            
            io:format("📱 [FCM] Registando token para user ~p~n", [UserId]),
            io:format("   Token: ~s...~n", [binary:part(FcmToken, 0, min(20, byte_size(FcmToken)))]),
            io:format("   Device: ~s~n", [DeviceType]),
            
            case save_fcm_token(UserId, FcmToken, DeviceType) of
                ok ->
                    io:format("✅ [FCM] Token registado com sucesso~n"),
                    Req2 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{success => true}), Req1),
                    {ok, Req2, State};
                {error, Reason} ->
                    io:format("❌ [FCM] Erro ao registar token: ~p~n", [Reason]),
                    Req2 = cowboy_req:reply(500, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => <<"failed_to_save">>}), Req1),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"fcm_token_required">>}), Req1),
            {ok, Req2, State}
    end;

%% POST /api/fcm/unregister
handle_request(<<"POST">>, <<"/api/fcm/unregister">>, UserId, Req0, State) ->
    {ok, Body, Req1} = cowboy_req:read_body(Req0),
    
    case jsx:decode(Body, [return_maps]) of
        #{<<"fcm_token">> := FcmToken} ->
            io:format("📱 [FCM] Removendo token para user ~p~n", [UserId]),
            
            case remove_fcm_token(UserId, FcmToken) of
                ok ->
                    io:format("✅ [FCM] Token removido com sucesso~n"),
                    Req2 = cowboy_req:reply(200, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{success => true}), Req1),
                    {ok, Req2, State};
                {error, Reason} ->
                    io:format("❌ [FCM] Erro ao remover token: ~p~n", [Reason]),
                    Req2 = cowboy_req:reply(500, #{
                        <<"content-type">> => <<"application/json">>
                    }, jsx:encode(#{error => <<"failed_to_remove">>}), Req1),
                    {ok, Req2, State}
            end;
        _ ->
            Req2 = cowboy_req:reply(400, #{
                <<"content-type">> => <<"application/json">>
            }, jsx:encode(#{error => <<"fcm_token_required">>}), Req1),
            {ok, Req2, State}
    end;

%% Método não suportado
handle_request(_, _, _, Req0, State) ->
    Req1 = cowboy_req:reply(405, #{
        <<"content-type">> => <<"application/json">>
    }, jsx:encode(#{error => <<"method_not_allowed">>}), Req0),
    {ok, Req1, State}.

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

%% Salvar token FCM no banco de dados
save_fcm_token(UserId, FcmToken, DeviceType) ->
    db_util:with_connection(fun(Conn) ->
        UserIdInt = binary_to_integer_safe(UserId),
        
        %% Primeiro, verificar se já existe um registro para este token
        %% Se existir, apenas atualizar. Se não, inserir novo.
        Sql = "INSERT INTO user_devices (user_id, device_uuid, device_type, push_token, last_active)
               VALUES ($1, $2, $3, $4, now())
               ON CONFLICT (user_id, device_uuid) DO UPDATE
               SET push_token = $4, device_type = $3, last_active = now()",
        
        %% Usar o hash do token como device_uuid para identificar dispositivo único
        DeviceUuid = crypto:hash(sha256, FcmToken),
        DeviceUuidHex = binary_to_hex(DeviceUuid),
        
        case epgsql:equery(Conn, Sql, [UserIdInt, DeviceUuidHex, DeviceType, FcmToken]) of
            {ok, _} -> ok;
            {ok, _, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).

%% Remover token FCM do banco de dados
remove_fcm_token(UserId, FcmToken) ->
    db_util:with_connection(fun(Conn) ->
        UserIdInt = binary_to_integer_safe(UserId),
        
        Sql = "DELETE FROM user_devices WHERE user_id = $1 AND push_token = $2",
        
        case epgsql:equery(Conn, Sql, [UserIdInt, FcmToken]) of
            {ok, _} -> ok;
            {error, Error} -> {error, Error}
        end
    end).

%% Converter binary para integer com segurança
binary_to_integer_safe(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin);
binary_to_integer_safe(Int) when is_integer(Int) ->
    Int.

%% Converter binary para hex string
binary_to_hex(Bin) ->
    << <<(integer_to_binary(X, 16))/binary>> || <<X>> <= Bin >>.

