%% -------------------------------------------------------------------
%% auth_util.erl
%% Funções de autenticação, sessões multi-dispositivo e integração Firebase
%% -------------------------------------------------------------------
-module(auth_util).

-export([
    verify_firebase_token/1,
    get_or_create_user/2,
    get_or_create_user/3, %% ✅✅✅ NOVO: Versão com nome do usuário
    create_session_token/1,
    select_or_insert_user/3,
    select_or_insert_user/4, %% ✅✅✅ NOVO: Versão com nome do usuário
    maybe_update_firebase_uid/3,
    %% Multi-device / Refresh logic
    create_session_for_user/3,
    generate_refresh_token/0,
    create_access_jwt/2,
    validate_and_rotate_refresh/2,
    revoke_other_sessions/2,
    revoke_session_by_token/1,
    %% JWT utilities
    decode_jwt/1,
    safe_to_integer/1,
    ensure_binary_utf8/1
]).

-include_lib("kernel/include/logger.hrl").

%% -------------------------------------------------------------------
%% 1) Verifica token do Firebase
%% -------------------------------------------------------------------
verify_firebase_token(IdToken) when is_binary(IdToken); is_list(IdToken) ->
    ApiKey = db_util:safe_env("FIREBASE_API_KEY", undefined),
    case ApiKey of
        undefined -> {error, "no_firebase_api_key_env"};
        _ ->
            application:ensure_all_started(inets),
            application:ensure_all_started(ssl),
            Url = "https://identitytoolkit.googleapis.com/v1/accounts:lookup?key=" ++ ApiKey,
            IdTokenBin =
                case IdToken of
                    I when is_list(I)  -> list_to_binary(I);
                    I when is_binary(I)-> I
                end,
            BodyJson = jsx:encode(#{<<"idToken">> => IdTokenBin}),
            Headers = [{"Content-Type", "application/json"}],
            case httpc:request(post, {Url, Headers, "application/json", BodyJson}, [], []) of
                {ok, {{_, 200, _}, _, RespBody}} ->
                    RespBin = ensure_binary_utf8(RespBody),
                    case jsx:decode(RespBin, [return_maps]) of
                        #{<<"users">> := [User | _]} -> {ok, User};
                        _ -> {error, invalid_response}
                    end;
                {ok, {{_, Code, _}, _, Body}} -> {error, {firebase_error, Code, Body}};
                {error, Reason} -> {error, {http_error, Reason}}
            end
    end.

%% -------------------------------------------------------------------
%% 2) Verifica/cria user no Postgres
%% -------------------------------------------------------------------
get_or_create_user(Phone, FirebaseUid) ->
    ?LOG_INFO("🔍 Getting or creating user for phone: ~s", [Phone]),
    db_util:with_connection(fun(Conn) ->
        select_or_insert_user(Conn, Phone, FirebaseUid)
    end).

%% ✅✅✅ NOVO: Versão com nome do usuário
get_or_create_user(Phone, FirebaseUid, UserName) ->
    ?LOG_INFO("🔍 Getting or creating user for phone: ~s, name: ~p", [Phone, UserName]),
    db_util:with_connection(fun(Conn) ->
        select_or_insert_user(Conn, Phone, FirebaseUid, UserName)
    end).

select_or_insert_user(Conn, Phone, FirebaseUid) ->
    PhoneBin = ensure_binary_utf8(Phone),
    FirebaseUidBin = ensure_binary_utf8(FirebaseUid),

    SqlSel = "SELECT id, name, phone, is_active, profile_picture FROM users WHERE phone = $1",
    case epgsql:equery(Conn, SqlSel, [PhoneBin]) of
        {ok, _, [Row | _]} ->
            {Id, Name, PhoneDb, IsActive, ProfilePic} = Row,
            _ = maybe_update_firebase_uid(Conn, Id, FirebaseUidBin),
            _ = epgsql:equery(Conn, "UPDATE users SET last_login = now() WHERE id = $1", [Id]),
            {ok, #{id => Id, name => Name, phone => PhoneDb, is_active => IsActive, profile_picture => ProfilePic}};
        {ok, _, []} ->
            %% ✅✅✅ CORREÇÃO DEFINITIVA: Sem caracteres especiais
            SqlIns = "INSERT INTO users (name, phone, is_active, created_at, firebase_uid)
                      VALUES ($1, $2, true, now(), $3)
                      RETURNING id, name, phone, is_active",
            case epgsql:equery(Conn, SqlIns, [<<"Usuario">>, PhoneBin, FirebaseUidBin]) of
                {ok, _, _, [Row | _]} ->
                    {Id, Name, PhoneDb, IsActive} = Row,
                    {ok, #{id => Id, name => Name, phone => PhoneDb, is_active => IsActive}};
                Err -> {error, {insert_failed, Err}}
            end;
        Err -> {error, {select_failed, Err}}
    end.

%% ✅✅✅ NOVO: Versão com nome do usuário
select_or_insert_user(Conn, Phone, FirebaseUid, UserName) ->
    PhoneBin = ensure_binary_utf8(Phone),
    FirebaseUidBin = ensure_binary_utf8(FirebaseUid),
    
    %% Determinar o nome a usar
    NameToUse = case UserName of
        undefined -> <<"Usuario">>; %% Nome padrão se não fornecido
        null -> <<"Usuario">>; %% Nome padrão se null
        Name when is_binary(Name) andalso byte_size(Name) > 0 -> Name; %% Usar nome fornecido
        Name when is_list(Name) andalso length(Name) > 0 -> ensure_binary_utf8(Name); %% Converter lista para binary
        _ -> <<"Usuario">> %% Fallback para nome padrão
    end,

    SqlSel = "SELECT id, name, phone, is_active, profile_picture FROM users WHERE phone = $1",
    case epgsql:equery(Conn, SqlSel, [PhoneBin]) of
        {ok, _, [Row | _]} ->
            %% ✅✅✅ Usuário existente: atualizar nome se fornecido e diferente
            {Id, NameFromDb, PhoneDb, IsActive, ProfilePic} = Row,
            _ = maybe_update_firebase_uid(Conn, Id, FirebaseUidBin),
            _ = epgsql:equery(Conn, "UPDATE users SET last_login = now() WHERE id = $1", [Id]),
            
            %% ✅✅✅ Atualizar nome se fornecido e diferente do atual
            case UserName of
                undefined -> ok; %% Não atualizar se não fornecido
                null -> ok; %% Não atualizar se null
                _ when NameToUse =/= NameFromDb ->
                    epgsql:equery(Conn, "UPDATE users SET name = $1 WHERE id = $2", [NameToUse, Id]),
                    ?LOG_INFO("✅ Updated user name from ~s to ~s", [NameFromDb, NameToUse]);
                _ -> ok %% Nome igual, não atualizar
            end,
            
            {ok, #{id => Id, name => NameToUse, phone => PhoneDb, is_active => IsActive, profile_picture => ProfilePic}};
            
        {ok, _, []} ->
            %% ✅✅✅ Usuário novo: criar com nome fornecido
            SqlIns = "INSERT INTO users (name, phone, is_active, created_at, firebase_uid)
                      VALUES ($1, $2, true, now(), $3)
                      RETURNING id, name, phone, is_active",
            case epgsql:equery(Conn, SqlIns, [NameToUse, PhoneBin, FirebaseUidBin]) of
                {ok, _, _, [Row | _]} ->
                    {Id, NameFromDb, PhoneDb, IsActive} = Row,
                    ?LOG_INFO("✅ Created new user with name: ~s", [NameToUse]),
                    {ok, #{id => Id, name => NameFromDb, phone => PhoneDb, is_active => IsActive}};
                Err -> {error, {insert_failed, Err}}
            end;
        Err -> {error, {select_failed, Err}}
    end.

maybe_update_firebase_uid(Conn, Id, FirebaseUid) when is_binary(FirebaseUid) ->
    epgsql:equery(Conn,
        "UPDATE users SET firebase_uid = $1 WHERE id = $2 AND (firebase_uid IS NULL OR firebase_uid = '')",
        [FirebaseUid, Id]),
    ok;
maybe_update_firebase_uid(_, _, _) -> ok.

%% -------------------------------------------------------------------
%% 3) Criação de JWT HS256
%% -------------------------------------------------------------------
create_session_token(UserMap) ->
    case os:getenv("SESSION_SECRET") of
        false -> {error, "no_session_secret_env"};
        SecretStr ->
            SecretBin = list_to_binary(SecretStr),
            Header = jsx:encode([{<<"alg">>, <<"HS256">>}, {<<"typ">>, <<"JWT">>}]),
            HBase = base64url(Header),

            Iat = erlang:system_time(second),
            Exp = Iat + 31536000,  % 365 dias
            
            %% ✅✅✅ CORREÇÃO: Incluir session_id nas claims
            BaseClaims = #{
                <<"user_id">> => ensure_binary_utf8(maps:get(id, UserMap, <<"unknown">>)),
                <<"phone">>   => ensure_binary_utf8(maps:get(phone, UserMap, <<"unknown">>)),
                <<"name">>    => ensure_binary_utf8(maps:get(name, UserMap, <<"unknown">>)),
                <<"iat">>     => Iat,
                <<"exp">>     => Exp
            },
            
            %% ✅ Adicionar session_id se estiver presente no UserMap
            Claims = case maps:get(session_id, UserMap, undefined) of
                undefined -> BaseClaims;
                SessionId -> BaseClaims#{<<"session_id">> => ensure_binary_utf8(SessionId)}
            end,
            
            PBase = base64url(jsx:encode(Claims)),
            SigningInput = <<HBase/binary, $., PBase/binary>>,
            Sig = base64url(crypto:mac(hmac, sha256, SecretBin, SigningInput)),
            Token = <<HBase/binary, $., PBase/binary, $., Sig/binary>>,
            {ok, binary_to_list(Token)}
    end.

%% -------------------------------------------------------------------
%% 4) Multi-device session management
%% -------------------------------------------------------------------
create_session_for_user(UserId, DeviceUUID, DeviceInfo) ->
    ?LOG_INFO("🆕 Creating session for user ~p, device ~s", [UserId, DeviceUUID]),
    Refresh = generate_refresh_token(),
    Hash = crypto:hash(sha256, Refresh),
    db_util:with_connection(fun(Conn) ->
        Sql = "INSERT INTO sessions (user_id, device_uuid, device_info, refresh_token_hash, created_at, expires_at)
               VALUES ($1, $2, $3, $4, now(), NULL) 
               RETURNING id",
        case epgsql:equery(Conn, Sql, [UserId, DeviceUUID, DeviceInfo, Hash]) of
            {ok, _, _, [{SessionId}]} -> 
                ?LOG_INFO("✅ Session created with ID: ~p", [SessionId]),
                {ok, #{session_id => SessionId, refresh_token => Refresh}};
            Err -> 
                ?LOG_ERROR("❌ Failed to create session: ~p", [Err]),
                {error, {insert_failed, Err}}
        end
    end).

generate_refresh_token() ->
    base64url(crypto:strong_rand_bytes(64)).

create_access_jwt(UserMap, SessionId) ->
    create_session_token(UserMap#{session_id => SessionId}).

validate_and_rotate_refresh(UserId, RefreshPlain) ->
    ?LOG_INFO("🔄 Validating and rotating refresh token for user ~p", [UserId]),
    RefreshHash = crypto:hash(sha256, RefreshPlain),
    db_util:with_connection(fun(Conn) ->
        SqlSel = "SELECT id FROM sessions WHERE user_id = $1 AND refresh_token_hash = $2 AND revoked = false",
        case epgsql:equery(Conn, SqlSel, [UserId, RefreshHash]) of
            {ok, _, [{SessionId}]} ->
                New = generate_refresh_token(),
                NewHash = crypto:hash(sha256, New),
                _ = epgsql:equery(Conn, "UPDATE sessions SET refresh_token_hash=$1,last_used_at=now() WHERE id=$2", [NewHash, SessionId]),
                ?LOG_INFO("✅ Refresh token rotated for session ~p", [SessionId]),
                {ok, #{session_id => SessionId, new_refresh => New}};
            {ok, _, []} -> 
                ?LOG_WARNING("⚠️ Invalid or expired refresh token for user ~p", [UserId]),
                {error, invalid_or_expired_refresh};
            Err -> 
                ?LOG_ERROR("❌ Database error during refresh validation: ~p", [Err]),
                {error, {db_error, Err}}
        end
    end).

revoke_other_sessions(UserId, KeepId) ->
    ?LOG_INFO("🚫 Revoking other sessions for user ~p, keeping session ~p", [UserId, KeepId]),
    
    try
        UserIdInt = safe_to_integer(UserId),
        KeepIdInt = safe_to_integer(KeepId),
        
        db_util:with_connection(fun(Conn) ->
            %% ✅✅✅ VERIFICAÇÃO EXTRA: Confirmar que a sessão a ser mantida pertence ao usuário
            SqlVerify = "SELECT user_id FROM sessions WHERE id = $1",
            case epgsql:equery(Conn, SqlVerify, [KeepIdInt]) of
                {ok, _, [{SessionUserId}]} when SessionUserId =:= UserIdInt ->
                    %% ✅ Sessão pertence ao usuário - PODE revogar outras
                    ?LOG_INFO("✅ Session ~p belongs to user ~p - proceeding with revocation", [KeepIdInt, UserIdInt]),
                    
                    SqlRevoke = "UPDATE sessions SET revoked=true WHERE user_id=$1 AND id<>$2",
                    case epgsql:equery(Conn, SqlRevoke, [UserIdInt, KeepIdInt]) of
                        {ok, Count} ->  
                            ?LOG_INFO("✅ Revoked ~p sessions for user ~p", [Count, UserIdInt]),
                            ok;
                        {error, Reason} ->
                            ?LOG_ERROR("❌ Failed to revoke sessions: ~p", [Reason]),
                            {error, {db_error, Reason}}
                    end;
                    
                {ok, _, [{OtherUserId}]} ->
                    %% ❌ Sessão NÃO pertence ao usuário - NÃO revoga!
                    ?LOG_ERROR("❌ SECURITY VIOLATION: Session ~p belongs to user ~p, but revocation requested by user ~p", 
                              [KeepIdInt, OtherUserId, UserIdInt]),
                    {error, security_violation};
                    
                {ok, _, []} ->
                    ?LOG_ERROR("❌ Session ~p not found", [KeepIdInt]),
                    {error, session_not_found};
                    
                {error, Reason} ->
                    ?LOG_ERROR("❌ Database error in session verification: ~p", [Reason]),
                    {error, Reason}
            end
        end)
    catch
        throw:{invalid_integer, Value} ->
            ?LOG_ERROR("❌ Invalid integer value: ~p", [Value]),
            {error, {invalid_integer, Value}};
        throw:{invalid_integer_type, Value} ->
            ?LOG_ERROR("❌ Invalid integer type: ~p", [Value]),
            {error, {invalid_integer_type, Value}};
        Class:Reason:Stack ->
            ?LOG_ERROR("❌ Unexpected error in revoke_other_sessions: ~p:~p ~p", [Class, Reason, Stack]),
            {error, {unexpected_error, Class, Reason}}
    end.

revoke_session_by_token(RefreshPlain) ->
    ?LOG_INFO("🚫 Revoking session by refresh token"),
    Hash = crypto:hash(sha256, RefreshPlain),
    db_util:with_connection(fun(Conn) ->
        case epgsql:equery(Conn, "UPDATE sessions SET revoked=true WHERE refresh_token_hash=$1", [Hash]) of
            {ok, Count} ->  % ✅ CORREÇÃO: {ok, Count} em vez de {ok, _, Count}
                ?LOG_INFO("✅ Revoked ~p sessions by token", [Count]),
                ok;
            {error, Reason} ->
                ?LOG_ERROR("❌ Failed to revoke session by token: ~p", [Reason]),
                {error, {db_error, Reason}}
        end
    end).

%% -------------------------------------------------------------------
%% 5) JWT Decoder (Simplified)
%% -------------------------------------------------------------------
decode_jwt(Token) when is_binary(Token); is_list(Token) ->
    try
        TokenBin = case Token of
            T when is_list(T) -> list_to_binary(T);
            T when is_binary(T) -> T
        end,
        
        %% Split JWT into parts
        Parts = binary:split(TokenBin, <<".">>, [global]),
        case Parts of
            [_, PayloadB64, _] ->
                %% Decode base64url (same as used in creation)
                Payload = base64url_decode(PayloadB64),
                case jsx:decode(Payload, [return_maps]) of
                    Claims when is_map(Claims) ->
                        %% Check expiration
                        Exp = maps:get(<<"exp">>, Claims, 0),
                        Now = erlang:system_time(second),
                        case Exp > Now of
                            true -> {ok, Claims};
                            false -> {error, token_expired}
                        end;
                    _ -> {error, invalid_payload}
                end;
            _ -> {error, invalid_jwt_format}
        end
    catch
        Class:Reason ->
            ?LOG_ERROR("❌ JWT decode error: ~p:~p", [Class, Reason]),
            {error, {decode_error, Class, Reason}}
    end.

base64url_decode(Bin) ->
    %% Add padding if needed
    Padding = case byte_size(Bin) rem 4 of
        0 -> <<>>;
        2 -> <<"==">>;
        3 -> <<"=">>
    end,
    Padded = <<Bin/binary, Padding/binary>>,
    %% Convert base64url to base64
    Base64 = binary:replace(binary:replace(Padded, <<"-">>, <<"+">>, [global]), <<"_">>, <<"/">>, [global]),
    base64:decode(Base64).

%% -------------------------------------------------------------------
%% Helpers
%% -------------------------------------------------------------------
base64url(Bin) when is_binary(Bin) ->
    Enc = base64:encode(Bin),
    Enc1 = binary:replace(Enc, <<"+">>, <<"-">>, [global]),
    Enc2 = binary:replace(Enc1, <<"/">>, <<"_">>, [global]),
    re:replace(Enc2, "=+$", "", [global, {return, binary}]);
base64url(Str) when is_list(Str) ->
    base64url(list_to_binary(Str)).

ensure_binary_utf8(Value) when is_binary(Value) -> Value;
ensure_binary_utf8(Value) when is_integer(Value) -> integer_to_binary(Value);
ensure_binary_utf8(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value, utf8, utf8)
    catch _:_ -> list_to_binary(Value)
    end;
ensure_binary_utf8(Other) ->
    try unicode:characters_to_binary(io_lib:format("~p", [Other]), utf8, utf8)
    catch _:_ -> <<"unknown">>
    end.

%% -------------------------------------------------------------------
%% Helper para converter para integer seguro
%% -------------------------------------------------------------------
safe_to_integer(Value) when is_binary(Value) ->
    try binary_to_integer(Value)
    catch _:_ -> 
        ?LOG_ERROR("❌ Cannot convert binary to integer: ~p", [Value]),
        throw({invalid_integer, Value})
    end;
safe_to_integer(Value) when is_list(Value) ->
    try list_to_integer(Value)
    catch _:_ -> 
        ?LOG_ERROR("❌ Cannot convert list to integer: ~p", [Value]),
        throw({invalid_integer, Value})
    end;
safe_to_integer(Value) when is_integer(Value) -> Value;
safe_to_integer(Value) ->
    ?LOG_ERROR("❌ Invalid type for integer conversion: ~p", [Value]),
    throw({invalid_integer_type, Value}).