%%%-------------------------------------------------------------------
%%% db_util.erl
%%% Módulo centralizado para gerenciamento de conexões com PostgreSQL
%%%-------------------------------------------------------------------

-module(db_util).

-export([
    get_connection/0,
    with_connection/1,
    safe_env/2,
    safe_env_int/2
]).

-include_lib("kernel/include/logger.hrl").

%% Defaults
-define(DEFAULT_DB_HOST, "localhost").
-define(DEFAULT_DB_PORT, 5432).
-define(DEFAULT_DB_NAME, "chat_app_db").
-define(DEFAULT_DB_USER, "postgres").

%%%-------------------------------------------------------------------
%%% API Pública
%%%-------------------------------------------------------------------

%% Obtém uma conexão com o banco
get_connection() ->
    Host = safe_env("DB_HOST", ?DEFAULT_DB_HOST),
    DbName = safe_env("DB_NAME", ?DEFAULT_DB_NAME),
    DbUser = safe_env("DB_USER", ?DEFAULT_DB_USER),
    DbPass = safe_env("DB_PASS", ""),
    Port = safe_env_int("DB_PORT", ?DEFAULT_DB_PORT),
    
    case epgsql:connect(Host, DbUser, DbPass, [{database, DbName}, {port, Port}]) of
        {ok, Conn} -> 
            ?LOG_INFO("✅ Database connection established to ~s:~p/~s", [Host, Port, DbName]),
            {ok, Conn};
        {error, Err} -> 
            ?LOG_ERROR("❌ Database connection failed: ~p", [Err]),
            {error, {db_connect_error, Err}}
    end.

%% Executa uma função com conexão automática
with_connection(Fun) when is_function(Fun, 1) ->
    case get_connection() of
        {ok, Conn} ->
            try
                Result = Fun(Conn),
                epgsql:close(Conn),
                Result
            catch
                Class:Reason:Stacktrace ->
                    epgsql:close(Conn),
                    ?LOG_ERROR("❌ Database operation failed: ~p:~p~nStacktrace: ~p", [Class, Reason, Stacktrace]),
                    {error, {db_operation_failed, Class, Reason}}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%%%-------------------------------------------------------------------
%%% Helpers
%%%-------------------------------------------------------------------

safe_env(Var, Default) ->
    case os:getenv(Var) of 
        false -> Default; 
        V -> V 
    end.

safe_env_int(Var, Default) ->
    case os:getenv(Var) of
        false -> Default;
        V when is_list(V) ->
            case catch list_to_integer(V) of
                N when is_integer(N) -> N;
                _ -> Default
            end;
        V when is_integer(V) -> V;
        _ -> Default
    end.
