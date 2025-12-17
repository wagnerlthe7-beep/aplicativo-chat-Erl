%%%-------------------------------------------------------------------
%%% logout_handler.erl - CORRIGIDO
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
            ?LOG_INFO("🔒 /auth/logout Body: ~p", [Body]),
            Dec = catch jsx:decode(Body, [return_maps]),
            case Dec of
                #{<<"refresh_token">> := Refresh} ->
                    Res = catch auth_util:revoke_session_by_token(Refresh),
                    ?LOG_INFO("🔒 revoke_session_by_token -> ~p", [Res]),
                    case Res of
                        ok ->
                            Response = jsx:encode(#{<<"success">> => true, <<"message">> => <<"Logged out successfully">>}),
                            Req2 = cowboy_req:reply(200, #{
                                <<"content-type">> => <<"application/json">>
                            }, Response, Req1),
                            {ok, Req2, State};
                        {ok, _} ->
                            Response = jsx:encode(#{<<"success">> => true, <<"message">> => <<"Logged out successfully">>}),
                            Req2 = cowboy_req:reply(200, #{
                                <<"content-type">> => <<"application/json">>
                            }, Response, Req1),
                            {ok, Req2, State};
                        _ ->
                            Response = jsx:encode(#{<<"success">> => false, <<"error">> => <<"Failed to revoke session">>}),
                            Req2 = cowboy_req:reply(500, #{
                                <<"content-type">> => <<"application/json">>
                            }, Response, Req1),
                            {ok, Req2, State}
                    end;
                _ ->
                    Response = jsx:encode(#{<<"success">> => false, <<"error">> => <<"Invalid payload">>}),
                    Req2 = cowboy_req:reply(400, #{
                        <<"content-type">> => <<"application/json">>
                    }, Response, Req1),
                    {ok, Req2, State}
            end;
        _ ->
            Response = jsx:encode(#{<<"success">> => false, <<"error">> => <<"Method not allowed">>}),
            Req2 = cowboy_req:reply(405, #{
                <<"content-type">> => <<"application/json">>
            }, Response, Req0),
            {ok, Req2, State}
    end.