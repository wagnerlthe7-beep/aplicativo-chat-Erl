-module(presence_handler).
-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"GET">> -> get_user_presence(Req0, State);
        _ -> 
            send_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, Req0, State}
    end.

get_user_presence(Req0, State) ->
    %% Definir ErrorResponse padrão antes do try
    DefaultErrorResponse =
        #{
            <<"status">> => <<"offline">>,
            <<"last_seen">> => null
        },
    
    try
        UserId = cowboy_req:binding(user_id, Req0),
        
        ?LOG_INFO("🔍 Buscando presença do usuário: ~p", [UserId]),
        
        Result = presence_manager:get_user_status(UserId),
        case Result of
            {ok, Status, LastSeen} ->
                %% ✅ CORREÇÃO IMPORTANTE:
                %% Se o Presence Manager diz "offline", mas o user_session ainda
                %% está com status "online" (WebSocket vivo), vamos CONFIAR
                %% no user_session e forçar "online". Isso evita o bug onde
                %% /api/presence/:id sempre devolve offline mesmo com o app aberto.
                EffectiveStatus =
                    case Status of
                        online ->
                            online;
                        offline ->
                            case catch user_session:get_status(UserId) of
                                {ok, online} ->
                                    ?LOG_INFO("✅ Override de presença: PresenceManager=offline mas user_session=online para ~p", [UserId]),
                                    online;
                                _ ->
                                    offline
                            end
                    end,

                Response =
                    case EffectiveStatus of
                        online ->
                            #{
                                <<"status">> => <<"online">>,
                                <<"last_seen">> => null
                            };
                        offline ->
                            LastSeenUnix =
                                try
                                    case LastSeen of
                                        null -> null;
                                        _ when is_tuple(LastSeen) ->
                                            %% É uma tupla datetime do Erlang
                                            %% Converter para timestamp Unix
                                            calendar:datetime_to_gregorian_seconds(LastSeen) - 62167219200;
                                        _ -> null
                                    end
                                catch
                                    _:_ ->
                                        ?LOG_WARNING("⚠️ Erro ao converter LastSeen: ~p", [LastSeen]),
                                        null
                                end,
                            #{
                                <<"status">> => <<"offline">>,
                                <<"last_seen">> => LastSeenUnix
                            }
                    end,
                send_json(Req0, 200, Response);
            {error, Reason} ->
                ?LOG_ERROR("❌ Erro ao buscar presença: ~p", [Reason]),
                %% Retornar offline em caso de erro
                send_json(Req0, 200, DefaultErrorResponse);
            Unexpected ->
                ?LOG_ERROR("❌ Resposta inesperada do presence_manager: ~p (Result original: ~p)", [Unexpected, Result]),
                %% Retornar offline em caso de formato inesperado
                send_json(Req0, 200, DefaultErrorResponse)
        end
    catch
        Error:Reason2 ->
            ?LOG_ERROR("❌ Erro em get_user_presence: ~p:~p", [Error, Reason2]),
            %% Retornar offline em caso de exceção
            send_json(Req0, 200, DefaultErrorResponse)
    end,
    {ok, Req0, State}.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).
