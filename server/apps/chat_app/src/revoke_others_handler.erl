%%%-------------------------------------------------------------------
%%% revoke_others_handler.erl
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
                            SessionId = maps:get(<<"session_id">>, Claims),
                            ?LOG_INFO("🚫 Revoking other sessions for user ~p, keeping session ~p", [UserId, SessionId]),
                            
                            case auth_util:revoke_other_sessions(UserId, SessionId) of
                                ok ->
                                    ?LOG_INFO("✅ Successfully revoked other sessions for user ~p", [UserId]),
                                    ReqF = firebase_handler:reply_json(Req1, 200, #{ok => <<"other_sessions_revoked">>}),
                                    {ok, ReqF, State};
                                {error, Reason} ->
                                    ?LOG_ERROR("❌ Failed to revoke other sessions: ~p", [Reason]),
                                    ReqF = firebase_handler:reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("revoke_failed:~p",[Reason]))}),
                                    {ok, ReqF, State}
                            end;
                        {error, Reason} ->
                            ?LOG_ERROR("❌ Failed to decode JWT: ~p", [Reason]),
                            ReqF = firebase_handler:reply_json(Req1, 401, #{error => list_to_binary(io_lib:format("jwt_decode_error:~p",[Reason]))}),
                            {ok, ReqF, State}
                    end;
                _ ->
                    ReqF = firebase_handler:reply_json(Req1, 400, #{error => <<"invalid_payload">>}),
                    {ok, ReqF, State}
            end;
        _ ->
            ReqF = firebase_handler:reply_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, ReqF, State}
    end.
