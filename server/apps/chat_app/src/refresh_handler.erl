%%%-------------------------------------------------------------------
%%% refresh_handler.erl
%%% Handler para /auth/refresh
%%%-------------------------------------------------------------------
-module(refresh_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            io:format("🔁 /auth/refresh Body: ~p~n", [Body]),
            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"refresh_token">> := _Refresh} ->
                    ?LOG_INFO("🔄 Refresh token request received"),
                    %% Para refresh, precisamos do user_id, mas não temos no token
                    %% Vamos precisar decodificar o JWT para obter o user_id
                    %% Por enquanto, vamos implementar uma versão simplificada
                    ReqF = firebase_handler:reply_json(Req1, 501, #{error => <<"refresh_not_implemented_yet">>}),
                    {ok, ReqF, State};
                _ ->
                    ReqF = firebase_handler:reply_json(Req1, 400, #{error => <<"invalid_payload">>}),
                    {ok, ReqF, State}
            end;
        _ ->
            ReqF = firebase_handler:reply_json(Req0, 405, #{error => <<"method_not_allowed">>}),
            {ok, ReqF, State}
    end.
