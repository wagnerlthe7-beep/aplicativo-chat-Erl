-module(db_pool).
-export([
    start_link/0, 
    stop/0, 
    with_connection/1, 
    get_connection/0, 
    return_connection/1
]).
-include_lib("kernel/include/logger.hrl").
start_link() ->
    Host = db_util:safe_env("DB_HOST", "localhost"),
    DbName = db_util:safe_env("DB_NAME", "chat_app_db"), 
    DbUser = db_util:safe_env("DB_USER", "postgres"),
    DbPass = db_util:safe_env("DB_PASS", ""),
    Port = db_util:safe_env_int("DB_PORT", 5432),
    
    PoolArgs = [
        {name, {local, db_pool}},
        {worker_module, db_worker},
        {size, 10},      % 10 conexões permanentes
        {max_overflow, 20} % +20 temporárias se necessário
    ],
    
    WorkerArgs = [{host, Host}, {port, Port}, {database, DbName}, 
                  {username, DbUser}, {password, DbPass}],
    
    poolboy:start_link(PoolArgs, WorkerArgs).

stop() ->
    poolboy:stop(db_pool).

with_connection(Fun) ->
    poolboy:transaction(db_pool, fun(Worker) ->
        {ok, Conn} = gen_server:call(Worker, get_connection),
        try
            Fun(Conn)
        after
            gen_server:cast(Worker, {return_connection, Conn})
        end
    end).

get_connection() ->
    poolboy:transaction(db_pool, fun(Worker) ->
        gen_server:call(Worker, get_connection)
    end).

return_connection(Conn) ->
    poolboy:transaction(db_pool, fun(Worker) ->
        gen_server:cast(Worker, {return_connection, Conn})
    end).
