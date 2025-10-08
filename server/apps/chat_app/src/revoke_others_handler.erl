-module(revoke_others_handler).
-export([init/2]).

init(Req0, State) ->
    ReqF = cowboy_req:reply(200, #{<<"content-type">> => <<"application/json">>}, <<"{\"ok\":\"test\"}">>, Req0),
    {ok, ReqF, State}.
