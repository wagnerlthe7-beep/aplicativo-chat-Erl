%%%-------------------------------------------------------------------
%%% logout_handler.erl
%%% Handler para /auth/logout (revoga 1 refresh token) 
%%%-------------------------------------------------------------------
-module(logout_handler).
-behaviour(cowboy_handler).

-export([init/2]).

-include_lib("kernel/include/logger.hrl").

init(Req0, State) ->
    Method = cowboy_req:method(Req0),
    case Method of
        <<"POST">> ->
            {ok, Body, Req1} = cowboy_req:read_body(Req0),
            io:format("🔒 /auth/logout Body: ~p~n", [Body]),
            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"refresh_token">> := Refresh} ->
                    Res = catch auth_util:revoke_session_by_token(Refresh),
                    io:format("🔒 revoke_session_by_token -> ~p~n", [Res]),
                    case Res of
                        ok ->
                            ReqF = firebase_handler:reply_json(Req1, 200, #{ok => <<"logged_out">>}),
                            {ok, ReqF, State};
                        {ok, _} ->
                            ReqF = firebase_handler:reply_json(Req1, 200, #{ok => <<"logged_out">>}),
                            {ok, ReqF, State};
                        _ ->
                            ReqF = firebase_handler:reply_json(Req1, 500, #{error => list_to_binary(io_lib:format("revoke_failed:~p",[Res]))}),
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
