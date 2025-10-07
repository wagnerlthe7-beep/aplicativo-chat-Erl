-module(pgsql_test).
-export([connect/0, query/1]).

connect() ->
    Host = "127.0.0.1",
    User = "postgres",
    Pass = "wagner21&",
    Db   = "chat_app_db",
    case epgsql:connect(Host, User, Pass, [
        {database, Db},
        {port, 5432},
        {ssl, false}
    ]) of
        {ok, Pid} ->
            io:format("Conectado com sucesso!~n"),
            Pid;
        Error ->
            io:format("Falha ao conectar: ~p~n", [Error]),
            Error
    end.

query(Sql) ->
    Pid = connect(),
    case epgsql:squery(Pid, Sql) of
        {ok, _Columns, Rows} ->
            lists:foreach(
              fun(Row) ->
                  io:format("~p~n", [Row])
              end,
              Rows),
            {ok, Rows};
        {error, Reason} ->
            io:format("Erro na query: ~p~n", [Reason]),
            {error, Reason}
    end.




