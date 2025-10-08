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
            %% Resposta simples sem usar firebase_handler:reply_json
            ReqF = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"{\"ok\":\"other_sessions_revoked\"}">>, Req1),
            {ok, ReqF, State};
        _ ->
            ReqF = cowboy_req:reply(405, #{<<"content-type">> => <<"application/json">>}, <<"{\"error\":\"method_not_allowed\"}">>, Req0),
            {ok, ReqF, State}
    end.
