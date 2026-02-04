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
                %% ✅ CONFIAR NO PRESENCE_MANAGER - ele já verifica heartbeat e WebSocket
                %% Não fazer override - se presence_manager diz offline, o usuário está offline
                EffectiveStatus = Status,

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
                                        {{Y, M, D}, {H, Min, Sec}} when is_float(Sec) ->
                                            %% ✅ PostgreSQL retorna com microssegundos (float)
                                            %% Converter para segundos inteiros primeiro
                                            SecInt = trunc(Sec),
                                            DateTime = {{Y, M, D}, {H, Min, SecInt}},
                                            calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200;
                                        {{Y, M, D}, {H, Min, Sec}} when is_integer(Sec) ->
                                            %% ✅ Já está em segundos inteiros
                                            DateTime = {{Y, M, D}, {H, Min, Sec}},
                                            calendar:datetime_to_gregorian_seconds(DateTime) - 62167219200;
                                        _ when is_tuple(LastSeen) ->
                                            %% ✅ Tentar conversão genérica (fallback)
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
        _:_ ->
            ?LOG_ERROR("❌ Erro em get_user_presence"),
            %% Retornar offline em caso de exceção
            send_json(Req0, 200, DefaultErrorResponse)
    end,
    {ok, Req0, State}.

send_json(Req, Status, Map) ->
    Json = jsx:encode(Map),
    cowboy_req:reply(Status, #{<<"content-type">> => <<"application/json">>}, Json, Req).
