%%%-------------------------------------------------------------------
%%% fcm_sender.erl - Módulo para enviar push notifications via FCM HTTP v1 API
%%% 
%%% Este módulo usa a API HTTP v1 do FCM (recomendada pela Google)
%%% Em vez de FCM_SERVER_KEY, usa Service Account JSON + OAuth2
%%%
%%% Configuração necessária (variável de ambiente):
%%%   GOOGLE_APPLICATION_CREDENTIALS - Caminho para o ficheiro service account JSON
%%%   ou
%%%   FCM_SERVICE_ACCOUNT_JSON - Conteúdo JSON do service account (alternativa)
%%%
%%% O ficheiro JSON deve conter:
%%%   - client_email
%%%   - private_key  
%%%   - project_id
%%%-------------------------------------------------------------------
-module(fcm_sender).

-export([
    send_message_notification/4,
    send_message_notification/5,
    send_data_notification/3,
    get_user_fcm_tokens/1
]).

-include_lib("kernel/include/logger.hrl").

%% Cache do access token (evitar pedir novo token a cada request)
-define(TOKEN_CACHE_KEY, fcm_access_token).
-define(TOKEN_EXPIRY_KEY, fcm_token_expiry).

%%%-------------------------------------------------------------------
%%% API Pública
%%%-------------------------------------------------------------------

%% @doc Enviar notificação de nova mensagem para um usuário
send_message_notification(ToUserId, FromUserId, MessageId, Content) ->
    send_message_notification(ToUserId, FromUserId, MessageId, Content, undefined).

send_message_notification(ToUserId, FromUserId, MessageId, Content, SenderName) ->
    io:format("📤 [FCM] Preparando notificação para user ~p~n", [ToUserId]),
    
    case get_user_fcm_tokens(ToUserId) of
        {ok, []} ->
            io:format("⚠️ [FCM] User ~p não tem tokens FCM registados~n", [ToUserId]),
            {error, no_tokens};
        {ok, Tokens} ->
            io:format("📱 [FCM] Encontrados ~p tokens para user ~p~n", [length(Tokens), ToUserId]),
            
            %% Enviar para todos os tokens do usuário
            Results = lists:map(fun(Token) ->
                send_to_token_v1(Token, FromUserId, MessageId, Content, SenderName)
            end, Tokens),
            
            %% Verificar resultados
            Successes = length([R || R <- Results, R == ok]),
            io:format("✅ [FCM] Enviadas ~p/~p notificações com sucesso~n", [Successes, length(Tokens)]),
            
            if Successes > 0 -> ok;
               true -> {error, all_failed}
            end;
        {error, Reason} ->
            io:format("❌ [FCM] Erro ao buscar tokens: ~p~n", [Reason]),
            {error, Reason}
    end.

%% @doc Enviar notificação de dados (silent push) para acordar a app
send_data_notification(ToUserId, MessageId, Data) ->
    io:format("📤 [FCM] Preparando data notification para user ~p~n", [ToUserId]),
    
    case get_user_fcm_tokens(ToUserId) of
        {ok, []} ->
            {error, no_tokens};
        {ok, Tokens} ->
            Results = lists:map(fun(Token) ->
                send_data_to_token_v1(Token, MessageId, Data)
            end, Tokens),
            
            Successes = length([R || R <- Results, R == ok]),
            if Successes > 0 -> ok;
               true -> {error, all_failed}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Obter todos os tokens FCM de um usuário
get_user_fcm_tokens(UserId) ->
    db_util:with_connection(fun(Conn) ->
        UserIdInt = binary_to_integer_safe(UserId),
        
        Sql = "SELECT push_token FROM user_devices 
               WHERE user_id = $1 AND push_token IS NOT NULL AND push_token != ''",
        
        case epgsql:equery(Conn, Sql, [UserIdInt]) of
            {ok, _, Rows} ->
                Tokens = [Token || {Token} <- Rows],
                {ok, Tokens};
            {error, Error} ->
                {error, Error}
        end
    end).

%%%-------------------------------------------------------------------
%%% FCM HTTP v1 API
%%%-------------------------------------------------------------------

%% Enviar notificação de mensagem para um token via HTTP v1
send_to_token_v1(Token, FromUserId, MessageId, Content, SenderName) ->
    case get_fcm_config() of
        {error, Reason} ->
            io:format("❌ [FCM] Configuração inválida: ~p~n", [Reason]),
            {error, Reason};
        {ok, ProjectId, _ServiceAccount} ->
            case get_access_token() of
                {error, TokenError} ->
                    io:format("❌ [FCM] Erro ao obter access token: ~p~n", [TokenError]),
                    {error, TokenError};
                {ok, AccessToken} ->
                    send_fcm_request(ProjectId, AccessToken, Token, FromUserId, MessageId, Content, SenderName)
            end
    end.

%% Enviar data notification para um token via HTTP v1
send_data_to_token_v1(Token, MessageId, Data) ->
    case get_fcm_config() of
        {error, Reason} ->
            {error, Reason};
        {ok, ProjectId, _ServiceAccount} ->
            case get_access_token() of
                {error, TokenError} ->
                    {error, TokenError};
                {ok, AccessToken} ->
                    send_fcm_data_request(ProjectId, AccessToken, Token, MessageId, Data)
            end
    end.

%% Fazer request HTTP para FCM v1
send_fcm_request(ProjectId, AccessToken, Token, FromUserId, MessageId, Content, SenderName) ->
    Url = "https://fcm.googleapis.com/v1/projects/" ++ binary_to_list(ProjectId) ++ "/messages:send",
    
    %% Limitar conteúdo para preview
    PreviewContent = case byte_size(Content) > 100 of
        true -> <<(binary:part(Content, 0, 97))/binary, "...">>;
        false -> Content
    end,
    
    Title = case SenderName of
        undefined -> <<"Nova mensagem">>;
        <<>> -> <<"Nova mensagem">>;
        Name -> Name
    end,
    
    %% Payload FCM v1 - Optimizado para entrega imediata e background
    %% ✅ IMPORTANTE: Usar apenas 'data' (sem 'notification') para evitar duplicação
    %% O Flutter cria notificação local customizada com ícone correto
    %% 'data' com priority=high é suficiente para acordar app em background
    Payload = #{
        <<"message">> => #{
            <<"token">> => Token,
            %% ✅ REMOVIDO: notification (causava duplicação e ícone padrão/castanho)
            %% O Flutter cria notificação local customizada via NotificationService
            <<"data">> => #{
                <<"type">> => <<"message">>,
                <<"message_id">> => ensure_binary(MessageId),
                <<"db_message_id">> => ensure_binary(MessageId),
                <<"sender_id">> => ensure_binary(FromUserId),
                <<"content">> => Content,
                <<"sender_name">> => ensure_binary(Title),
                <<"click_action">> => <<"FLUTTER_NOTIFICATION_CLICK">>
            },
            <<"android">> => #{
                <<"priority">> => <<"high">>,  %% ✅ Priority high acorda app mesmo sem 'notification'
                <<"ttl">> => <<"86400s">>,  %% 24 horas (não 0s para permitir entrega atrasada)
                <<"direct_boot_ok">> => true  %% Permite entrega antes do unlock
                %% ✅ REMOVIDO: notification (causava duplicação e ícone padrão/castanho)
                %% O Flutter cria notificação local customizada via NotificationService
            },
            <<"apns">> => #{
                <<"headers">> => #{
                    <<"apns-priority">> => <<"10">>,  %% Máxima prioridade iOS
                    <<"apns-push-type">> => <<"alert">>
                },
                <<"payload">> => #{
                    <<"aps">> => #{
                        <<"alert">> => #{
                            <<"title">> => Title,
                            <<"body">> => PreviewContent
                        },
                        <<"sound">> => <<"default">>,
                        <<"badge">> => 1,
                        <<"interruption-level">> => <<"time-sensitive">>
                    }
                }
            }
        }
    },
    
    do_fcm_http_request(Url, AccessToken, Payload, Token).

%% Fazer request HTTP para data notification
send_fcm_data_request(ProjectId, AccessToken, Token, MessageId, Data) ->
    Url = "https://fcm.googleapis.com/v1/projects/" ++ binary_to_list(ProjectId) ++ "/messages:send",
    
    Payload = #{
        <<"message">> => #{
            <<"token">> => Token,
            <<"data">> => Data#{
                <<"message_id">> => ensure_binary(MessageId)
            },
            <<"android">> => #{
                <<"priority">> => <<"high">>
            },
            <<"apns">> => #{
                <<"payload">> => #{
                    <<"aps">> => #{
                        <<"content-available">> => 1
                    }
                },
                <<"headers">> => #{
                    <<"apns-priority">> => <<"10">>
                }
            }
        }
    },
    
    do_fcm_http_request(Url, AccessToken, Payload, Token).

%% Executar o request HTTP
do_fcm_http_request(Url, AccessToken, Payload, Token) ->
    JsonPayload = jsx:encode(Payload),
    
    Headers = [
        {"Authorization", "Bearer " ++ binary_to_list(AccessToken)},
        {"Content-Type", "application/json"}
    ],
    
    case httpc:request(post, {Url, Headers, "application/json", JsonPayload}, 
                      [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _, _ResponseBody}} ->
            io:format("✅ [FCM v1] Notificação enviada com sucesso~n"),
            ok;
        {ok, {{_, 404, _}, _, ResponseBody}} ->
            io:format("⚠️ [FCM v1] Token não encontrado (404): ~s~n", [ResponseBody]),
            handle_invalid_token(Token),
            {error, not_found};
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("❌ [FCM v1] Erro HTTP ~p: ~s~n", [StatusCode, ResponseBody]),
            %% Verificar se é erro de token inválido ou SenderId mismatch
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"error">> := #{<<"details">> := Details}} ->
                    check_token_errors(Details, Token);
                #{<<"error">> := #{<<"code">> := 403, <<"message">> := <<"SenderId mismatch", _/binary>>}} ->
                    %% Erro 403: SenderId mismatch - token foi gerado para outro projeto Firebase
                    io:format("⚠️ [FCM] SenderId mismatch - token inválido (projeto Firebase diferente)~n"),
                    handle_invalid_token(Token),
                    {error, sender_id_mismatch};
                _ ->
                    ok
            end,
            {error, {http_error, StatusCode}};
        {error, Reason} ->
            io:format("❌ [FCM v1] Erro de conexão: ~p~n", [Reason]),
            {error, Reason}
    end.

%% Verificar erros de token nos detalhes do erro
check_token_errors([], _Token) -> ok;
check_token_errors([#{<<"errorCode">> := <<"UNREGISTERED">>} | _], Token) ->
    handle_invalid_token(Token);
check_token_errors([#{<<"errorCode">> := <<"INVALID_ARGUMENT">>} | _], Token) ->
    handle_invalid_token(Token);
check_token_errors([#{<<"errorCode">> := <<"SENDER_ID_MISMATCH">>} | _], Token) ->
    %% Erro específico: token foi gerado para outro projeto Firebase
    io:format("⚠️ [FCM] SenderId mismatch detectado nos detalhes~n"),
    handle_invalid_token(Token);
check_token_errors([_ | Rest], Token) ->
    check_token_errors(Rest, Token).

%% Remover token inválido
handle_invalid_token(Token) ->
    io:format("🗑️ [FCM] Removendo token inválido~n"),
    db_util:with_connection(fun(Conn) ->
        Sql = "DELETE FROM user_devices WHERE push_token = $1",
        epgsql:equery(Conn, Sql, [Token])
    end).

%%%-------------------------------------------------------------------
%%% OAuth2 / Service Account
%%%-------------------------------------------------------------------

%% Obter configuração do FCM (project_id e service account)
get_fcm_config() ->
    case os:getenv("GOOGLE_APPLICATION_CREDENTIALS") of
        false ->
            %% Tentar variável alternativa com JSON inline
            case os:getenv("FCM_SERVICE_ACCOUNT_JSON") of
                false ->
                    {error, no_credentials};
                JsonStr ->
                    parse_service_account(list_to_binary(JsonStr))
            end;
        FilePath ->
            case file:read_file(FilePath) of
                {ok, JsonBin} ->
                    parse_service_account(JsonBin);
                {error, Reason} ->
                    io:format("❌ [FCM] Erro ao ler ficheiro de credenciais: ~p~n", [Reason]),
                    {error, {file_error, Reason}}
            end
    end.

%% Parsear o JSON do service account
parse_service_account(JsonBin) ->
    try
        ServiceAccount = jsx:decode(JsonBin, [return_maps]),
        ProjectId = maps:get(<<"project_id">>, ServiceAccount),
        {ok, ProjectId, ServiceAccount}
    catch
        _:_ ->
            {error, invalid_json}
    end.

%% Obter access token (com cache)
get_access_token() ->
    %% Verificar cache
    case get_cached_token() of
        {ok, Token} ->
            {ok, Token};
        expired ->
            %% Token expirado, obter novo
            refresh_access_token()
    end.

%% Verificar se há token em cache e se ainda é válido
get_cached_token() ->
    case application:get_env(chat_app, ?TOKEN_CACHE_KEY) of
        undefined ->
            expired;
        {ok, Token} ->
            case application:get_env(chat_app, ?TOKEN_EXPIRY_KEY) of
                undefined ->
                    expired;
                {ok, Expiry} ->
                    Now = erlang:system_time(second),
                    if Expiry > Now + 60 ->  %% 60 segundos de margem
                        {ok, Token};
                    true ->
                        expired
                    end
            end
    end.

%% Obter novo access token via OAuth2
refresh_access_token() ->
    case get_fcm_config() of
        {error, Reason} ->
            {error, Reason};
        {ok, _ProjectId, ServiceAccount} ->
            ClientEmail = maps:get(<<"client_email">>, ServiceAccount),
            PrivateKey = maps:get(<<"private_key">>, ServiceAccount),
            
            %% Criar JWT para OAuth2
            case create_oauth_jwt(ClientEmail, PrivateKey) of
                {error, JwtError} ->
                    {error, JwtError};
                {ok, Jwt} ->
                    %% Trocar JWT por access token
                    exchange_jwt_for_token(Jwt)
            end
    end.

%% Criar JWT para autenticação OAuth2
create_oauth_jwt(ClientEmail, PrivateKey) ->
    try
        Now = erlang:system_time(second),
        Expiry = Now + 3600,  %% 1 hora
        
        %% Header
        Header = jsx:encode(#{
            <<"alg">> => <<"RS256">>,
            <<"typ">> => <<"JWT">>
        }),
        HeaderB64 = base64url_encode(Header),
        
        %% Payload (claims)
        Payload = jsx:encode(#{
            <<"iss">> => ClientEmail,
            <<"sub">> => ClientEmail,
            <<"aud">> => <<"https://oauth2.googleapis.com/token">>,
            <<"iat">> => Now,
            <<"exp">> => Expiry,
            <<"scope">> => <<"https://www.googleapis.com/auth/firebase.messaging">>
        }),
        PayloadB64 = base64url_encode(Payload),
        
        %% Signing input
        SigningInput = <<HeaderB64/binary, ".", PayloadB64/binary>>,
        
        %% Assinar com a chave privada
        case sign_with_rsa(SigningInput, PrivateKey) of
            {error, SignError} ->
                {error, SignError};
            {ok, Signature} ->
                SignatureB64 = base64url_encode(Signature),
                Jwt = <<SigningInput/binary, ".", SignatureB64/binary>>,
                {ok, Jwt}
        end
    catch
        Error:Reason ->
            io:format("❌ [FCM] Erro ao criar JWT: ~p:~p~n", [Error, Reason]),
            {error, {jwt_creation_failed, Reason}}
    end.

%% Assinar dados com RSA SHA256
sign_with_rsa(Data, PrivateKeyPem) ->
    try
        %% Decodificar a chave PEM
        [PemEntry] = public_key:pem_decode(PrivateKeyPem),
        PrivateKey = public_key:pem_entry_decode(PemEntry),
        
        %% Assinar
        Signature = public_key:sign(Data, sha256, PrivateKey),
        {ok, Signature}
    catch
        Error:Reason ->
            io:format("❌ [FCM] Erro ao assinar: ~p:~p~n", [Error, Reason]),
            {error, {signing_failed, Reason}}
    end.

%% Trocar JWT por access token
exchange_jwt_for_token(Jwt) ->
    Url = "https://oauth2.googleapis.com/token",
    
    Body = "grant_type=urn%3Aietf%3Aparams%3Aoauth%3Agrant-type%3Ajwt-bearer&assertion=" ++ binary_to_list(Jwt),
    
    Headers = [
        {"Content-Type", "application/x-www-form-urlencoded"}
    ],
    
    case httpc:request(post, {Url, Headers, "application/x-www-form-urlencoded", Body}, 
                      [{timeout, 10000}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"access_token">> := AccessToken, <<"expires_in">> := ExpiresIn} ->
                    %% Guardar em cache
                    Expiry = erlang:system_time(second) + ExpiresIn,
                    application:set_env(chat_app, ?TOKEN_CACHE_KEY, AccessToken),
                    application:set_env(chat_app, ?TOKEN_EXPIRY_KEY, Expiry),
                    io:format("✅ [FCM] Access token obtido (expira em ~p segundos)~n", [ExpiresIn]),
                    {ok, AccessToken};
                _ ->
                    {error, invalid_token_response}
            end;
        {ok, {{_, StatusCode, _}, _, ResponseBody}} ->
            io:format("❌ [FCM] Erro ao obter token: ~p - ~s~n", [StatusCode, ResponseBody]),
            {error, {token_exchange_failed, StatusCode}};
        {error, Reason} ->
            io:format("❌ [FCM] Erro de conexão ao obter token: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% Funções auxiliares
%%%-------------------------------------------------------------------

%% Base64 URL encoding (sem padding)
base64url_encode(Data) ->
    B64 = base64:encode(Data),
    %% Substituir caracteres não-URL-safe e remover padding
    B64_1 = binary:replace(B64, <<"+">>, <<"-">>, [global]),
    B64_2 = binary:replace(B64_1, <<"/">>, <<"_">>, [global]),
    binary:replace(B64_2, <<"=">>, <<>>, [global]).

%% Converter para binary de forma segura
ensure_binary(Val) when is_binary(Val) -> Val;
ensure_binary(Val) when is_integer(Val) -> integer_to_binary(Val);
ensure_binary(Val) when is_list(Val) -> list_to_binary(Val);
ensure_binary(Val) when is_atom(Val) -> atom_to_binary(Val, utf8);
ensure_binary(_) -> <<"">>.

%% Converter binary para integer com segurança
binary_to_integer_safe(Bin) when is_binary(Bin) ->
    binary_to_integer(Bin);
binary_to_integer_safe(Int) when is_integer(Int) ->
    Int.
