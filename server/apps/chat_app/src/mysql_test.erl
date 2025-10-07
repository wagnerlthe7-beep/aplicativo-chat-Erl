-module(mysql_test).
-export([connect/0]).


connect() ->
    case mysql:start_link([
        {host, "localhost"},
        {port, 3307},
        {user, "root"},
        {password, "wagner21&3307"},
        {database, "chat_app"}
    ]) of
        {ok, Pid} ->
            io:format("Conectado com sucesso!~n"),
            Pid;
        Pid when is_pid(Pid) ->
            io:format("Conectado com sucesso!~n"),
            Pid;
        Error ->
            io:format("Falha ao conectar: ~p~n", [Error]),
            Error
    end.
