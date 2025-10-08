%%%-------------------------------------------------------------------
%% @doc chat_app public API
%% @end
%%%-------------------------------------------------------------------

%% chat_app_app.erl
%% Inicializa o servidor Cowboy e registra rotas REST

-module(chat_app_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% Application callbacks
%% ------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    %% Dispatcher com rotas
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/auth/firebase", firebase_handler, []},
            {"/auth/refresh", refresh_handler, []},
            {"/auth/logout", logout_handler, []}
            %% Temporariamente removido: {"/auth/revoke-others", revoke_others_handler, []}
            %% Adicione outras rotas aqui, ex: {["/chat"], [], chat_handler, []}
        ]}
    ]),

    %% Configura o listener HTTP
    {ok, _Pid} = cowboy:start_clear(
        http_listener,           %% nome do listener
        [{port, 4000}, {ip, {0,0,0,0}}],         %% porta e IP
        #{env => #{dispatch => Dispatch}}  %% configuração do dispatcher
    ),

    io:format("🚀 Chat_app server listening on port 4000~n", []),

    {ok, self()}.

stop(_State) ->
    ok.

