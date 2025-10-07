%% INICIALIZA COWBOY E DEFINE ROTAS

-module(server).
-export([start/0]).

start() ->
    %% 1. Definir rotas
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/auth/firebase", firebase_handler, []}
        ]}
    ]),

    %% 2. Opções do listener
    ListenOpts = #{port => 4000},
    EnvOpts = #{env => #{dispatch => Dispatch}},

    %% 3. Iniciar listener
    case cowboy:start_clear(http_listener, ListenOpts, EnvOpts) of
        {ok, _Pid} ->
            io:format("Servidor iniciado na porta 4000~n"),
            ok;
        {error, Reason} ->
            io:format("Falha ao iniciar servidor: ~p~n", [Reason]),
            {error, Reason}
    end.


