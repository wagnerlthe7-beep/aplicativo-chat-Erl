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

%% NOVA versão - usar pool em vez de conexões diretas
with_connection(Fun) when is_function(Fun, 1) ->
    db_pool:with_connection(Fun).  % ✅ Delega para o pool

get_connection() ->
    db_pool:get_connection().  % ✅ Delega para o pool


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