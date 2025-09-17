%%%-------------------------------------------------------------------
%% @doc chat_app public API
%% @end
%%%-------------------------------------------------------------------

-module(chat_app_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    chat_app_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
