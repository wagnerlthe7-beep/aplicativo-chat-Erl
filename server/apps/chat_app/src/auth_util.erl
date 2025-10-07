%% -------------------------------------------------------------------
%% auth_util.erl
%% Funções de validação Firebase, Postgres e criação de token de sessão
%% -------------------------------------------------------------------
-module(auth_util).
-export([verify_firebase_token/1, get_or_create_user/2, create_session_token/1, select_or_insert_user/3, maybe_update_firebase_uid/3]).


-include_lib("kernel/include/logger.hrl").

%% Defaults
-define(DEFAULT_DB_HOST, "localhost").
-define(DEFAULT_DB_PORT, 5432).
-define(DEFAULT_DB_NAME, "chat_app_db").
-define(DEFAULT_DB_USER, "postgres").

verify_firebase_token(IdToken) when is_binary(IdToken); is_list(IdToken) ->
    ApiKey0 = os:getenv("FIREBASE_API_KEY"),
    ApiKey = case ApiKey0 of false -> undefined; V -> V end,
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
            BodyMap = #{<<"idToken">> => IdTokenBin},
            BodyJson = jsx:encode(BodyMap),
            Headers = [{"Content-Type", "application/json"}],
            Request = {Url, Headers, "application/json", BodyJson},
            case httpc:request(post, Request, [], []) of
                {ok, {{_, 200, _}, _RespHeaders, RespBody}} ->
                    io:format("🔍 RespBody bruto do Firebase: ~p~n", [RespBody]),
                    RespBodyBin =
                        case RespBody of
                            B when is_binary(B) -> B;
                            L when is_list(L)   -> list_to_binary(L)
                        end,
                    Dec = (catch jsx:decode(RespBodyBin, [return_maps])),
                    case Dec of
                        Map when is_map(Map) ->
                            Users = maps:get(<<"users">>, Map, undefined),
                            case Users of
                                undefined -> {error, "no_users_in_response"};
                                [First|_] when is_map(First) -> {ok, First};
                                _ -> {error, "unexpected_users_format"}
                            end;
                        _ ->
                            {error, {invalid_json_response, RespBody}}
                    end;
                {ok, {{_, Code, _}, _RespHeaders, RespBody}} ->
                    {error, {firebase_api_error, Code, RespBody}};
                {error, Reason} ->
                    {error, {http_error, Reason}}
            end
    end.

%%% -------------------------------------------------------------------
%%% 2) Verifica/cria user no Postgres (versão revisada)
%%% -------------------------------------------------------------------
get_or_create_user(Phone, FirebaseUid) ->
    io:format("🔹 Entrou em get_or_create_user: ~p ~p~n", [Phone, FirebaseUid]),
    try
        %% Lê variáveis de ambiente ou usa defaults
        Host = case os:getenv("DB_HOST") of false -> "localhost"; V -> V end,
        DbName = case os:getenv("DB_NAME") of false -> "chat_app_db"; T -> T end,
        DbUser = case os:getenv("DB_USER") of false -> "postgres"; P -> P end,
        DbPass = case os:getenv("DB_PASS") of false -> "wagner21&"; Q -> Q end,
        Port =
            case os:getenv("DB_PORT") of
                false -> 5432;
                Vstr when is_list(Vstr) ->
                    case catch list_to_integer(Vstr) of
                        N when is_integer(N) -> N;
                        _ -> 5432
                    end;
                Vint when is_integer(Vint) -> Vint;
                _ -> 5432
            end,

        io:format("DEBUG >> Host=~p, DbUser=~p, DbPass=~p, DbName=~p, Port=~p~n",
                  [Host, DbUser, DbPass, DbName, Port]),

        %% Conecta ao Postgres
        case epgsql:connect(Host, DbUser, DbPass, [{database, DbName}, {port, Port}]) of
            {ok, Conn} ->
                io:format("🔹 Conectou: ~p ~p~n", [Phone, FirebaseUid]),
                select_or_insert_user(Conn, Phone, FirebaseUid);
            {error, Err} ->
                {error, {db_connect_error, Err}}
        end
    catch
        Class:Reason ->
            {error, {exception, Class, Reason}}
    end.

%%% -------------------------------------------------------------------
%%% Auxiliares Postgres
%%% -------------------------------------------------------------------
select_or_insert_user(Conn, Phone, FirebaseUid) ->
    %% Garante que Phone e FirebaseUid sejam binaries UTF-8
    PhoneBin = ensure_binary_utf8(Phone),
    FirebaseUidBin = ensure_binary_utf8(FirebaseUid),

    %% 1) Verifica se o usuário já existe
    SelectSql = "SELECT id, name, phone, is_active, profile_picture FROM users WHERE phone = $1",
    case epgsql:equery(Conn, SelectSql, [PhoneBin]) of
        {ok, _Cols, Rows} when Rows =/= [] ->
            case hd(Rows) of
                {Id, Name, PhoneDb, IsActive, ProfilePicture} ->
                    _ = maybe_update_firebase_uid(Conn, Id, FirebaseUidBin),
                    _ = epgsql:equery(Conn, "UPDATE users SET last_login = now() WHERE id = $1", [Id]),
                    {ok, #{id => Id, name => Name, phone => PhoneDb, is_active => IsActive, profile_picture => ProfilePicture}};
                _ -> {error, unexpected_row_format}
            end;

        {ok, _Cols, []} ->
            %% Usuário não existe → cria
            DefaultName = <<"Usuário"/utf8>>,
            InsertSql = "INSERT INTO users (name, phone, is_active, created_at, firebase_uid) VALUES ($1, $2, true, now(), $3) RETURNING id, name, phone, is_active",
            case epgsql:equery(Conn, InsertSql, [DefaultName, PhoneBin, FirebaseUidBin]) of
                {ok, _Count, _Cols2, InsertRows} when InsertRows =/= [] ->
                    case hd(InsertRows) of
                        {Id, Name, PhoneDb, IsActive} ->
                            {ok, #{id => Id, name => Name, phone => PhoneDb, is_active => IsActive}};
                        _ -> {error, unexpected_insert_format}
                    end;
                {ok, _Count} ->
                    {ok, #{created_rows => _Count}};
                {error, Err} -> {error, {db_insert_error, Err}};
                Other -> {error, {insert_unexpected, Other}}
            end;

        {error, Err} -> {error, {db_select_error, Err}};
        Other -> {error, {unexpected_equery_result, Other}}
    end.


maybe_update_firebase_uid(_Conn, _Id, undefined) -> ok;
maybe_update_firebase_uid(Conn, Id, FirebaseUid) when is_binary(FirebaseUid); is_list(FirebaseUid) ->
    catch epgsql:equery(Conn,
        "UPDATE users SET firebase_uid = $1 WHERE id = $2 AND (firebase_uid IS NULL OR firebase_uid = '')",
        [FirebaseUid, Id]),
    ok;
maybe_update_firebase_uid(_, _, _) -> ok.

%%% -------------------------------------------------------------------
%%% 3) Criar JWT HS256 seguro (versão UTF-8 segura)
%%% -------------------------------------------------------------------
create_session_token(UserMap) when is_map(UserMap) ->
    io:format("🔹 Criando session token para: ~p~n", [UserMap]),
    try
        case os:getenv("SESSION_SECRET") of
            false ->
                {error, "no_session_secret_env"};
            SecretStr ->
                io:format("🔹 SecretStr: ~p~n", [SecretStr]),
                SecretBin = list_to_binary(SecretStr),
                io:format("🔹 SecretBin: ~p~n", [SecretBin]),

                Header = [{<<"alg">>, <<"HS256">>}, {<<"typ">>, <<"JWT">>}],
                HJson = jsx:encode(Header),
                io:format("🔹 HJson: ~p~n", [HJson]),
                HJsonBin = iolist_to_binary(HJson),
                HBase = base64url(HJsonBin),

                io:format("🔹 HBase: ~p~n", [HBase]),

                %% Timestamps
                Iat = erlang:system_time(second),
                Exp = Iat + 3600,

                %% Garantir UTF-8 seguro para todos os campos
                UserIdRaw = maps:get(id, UserMap, <<"unknown">>),
                PhoneRaw  = maps:get(phone, UserMap, <<"unknown">>),
                NameRaw   = maps:get(name, UserMap, <<"unknown">>),

                io:format("🔹 Raw values -> id: ~p, phone: ~p, name: ~p~n",
                          [UserIdRaw, PhoneRaw, NameRaw]),

                UserIdBin = ensure_binary_utf8(UserIdRaw),
                PhoneBin  = ensure_binary_utf8(PhoneRaw),
                NameBin   = ensure_binary_utf8(NameRaw),

                io:format("🔹 After ensure_binary_utf8 -> id: ~p, phone: ~p, name: ~p~n",
                          [UserIdBin, PhoneBin, NameBin]),

                ClaimsMap = #{
                    <<"user_id">> => UserIdBin,
                    <<"phone">>   => PhoneBin,
                    <<"name">>    => NameBin,
                    <<"iat">>     => Iat,
                    <<"exp">>     => Exp
                },

                io:format("🔹 ClaimsMap: ~p~n", [ClaimsMap]),
                PJson = jsx:encode(ClaimsMap),
                io:format("🔹 PJson: ~p~n", [PJson]),
                PBase = base64url(PJson),
                io:format("🔹 PBase: ~p~n", [PBase]),

                SigningInput = <<HBase/binary, $., PBase/binary>>,
                io:format("🔹 SigningInput: ~p~n", [SigningInput]),

                SigRaw = crypto:mac(hmac, sha256, SecretBin, SigningInput),
                io:format("🔹 SigRaw: ~p~n", [SigRaw]),
                SigBase = base64url(SigRaw),
                io:format("🔹 SigBase: ~p~n", [SigBase]),

                TokenBin = <<HBase/binary, $., PBase/binary, $., SigBase/binary>>,
                io:format("🔹 TokenBin: ~p~n", [TokenBin]),
                {ok, binary_to_list(TokenBin)}
        end
    catch
        Class:Reason:Stack ->
            io:format("❌ ERRO em create_session_token: ~p ~p~nStack: ~p~n",
                      [Class, Reason, Stack]),
            {error, {exception, Class, Reason}}
    end.


%%% -------------------------------------------------------------------
%%% Base64url helper
%%% -------------------------------------------------------------------
base64url(Bin) when is_binary(Bin) ->
    Enc = base64:encode(Bin),
    Enc1 = binary:replace(Enc, <<"+">>, <<"-">>, [global]),
    Enc2 = binary:replace(Enc1, <<"/">>, <<"_">>, [global]),
    re:replace(Enc2, "=+$", "", [global, {return, binary}]);
base64url(Str) when is_list(Str) -> base64url(list_to_binary(Str)).


%%% -------------------------------------------------------------------
%%% Helper para forçar UTF-8
%%% -------------------------------------------------------------------
ensure_binary_utf8(Value) when is_binary(Value) ->
    Value;
ensure_binary_utf8(Value) when is_integer(Value) ->
    integer_to_binary(Value);
ensure_binary_utf8(Value) when is_list(Value) ->
    try unicode:characters_to_binary(Value, utf8, utf8)
    catch _:_ -> list_to_binary(Value)
    end;
ensure_binary_utf8(Other) ->
    try unicode:characters_to_binary(io_lib:format("~p", [Other]), utf8, utf8)
    catch _:_ -> <<"unknown">>
    end.




%%% -------------------------------------------------------------------
%%% Helpers para env
%%% -------------------------------------------------------------------
safe_env(Var, Default) ->
    case os:getenv(Var) of false -> Default; V -> V end.

safe_env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        V when is_list(V) ->
            (catch list_to_integer(V)) orelse Default;
        V when is_integer(V) -> V;
        _ -> Default
    end.
