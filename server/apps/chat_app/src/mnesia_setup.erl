-module(mnesia_setup).
-export([init/0, create_tables/0, clear_tables/0]).

%% Records definitions - DEFINIR PRIMEIRO!
-record(online_users, {
    user_id,
    ws_pid,
    status,
    last_seen
}).

-record(pending_messages, {
    message_id,
    receiver_id, 
    sender_id,
    content,
    timestamp,
    status
}).

init() ->
    Node = node(),
    io:format("🔧 Initializing Mnesia on node: ~p~n", [Node]),
    
    %% Inicializar Mnesia
    case mnesia:create_schema([Node]) of
        ok -> 
            io:format("✅ Mnesia schema created on ~p~n", [Node]);
        {error, {_, {already_exists, _}}} ->
            io:format("✅ Mnesia schema already exists on ~p~n", [Node]);
        Error ->
            io:format("❌ Mnesia schema error: ~p~n", [Error])
    end,
    
    %% Iniciar Mnesia
    case mnesia:start() of
        ok ->
            io:format("✅ Mnesia started on ~p~n", [Node]),
            create_tables();
        {error, Reason} ->
            io:format("❌ Mnesia start error: ~p~n", [Reason])
    end.

create_tables() ->
    Node = node(),
    Tables = [
        {online_users, [
            {type, set},
            {ram_copies, [Node]},  %% Usar Node atual
            {attributes, [user_id, ws_pid, status, last_seen]}
        ]},
        {pending_messages, [
            {type, bag}, 
            {ram_copies, [Node]},  %% Usar Node atual
            {attributes, [message_id, receiver_id, sender_id, content, timestamp, status]}
        ]}
    ],
    
    lists:foreach(fun({TableName, TableDef}) ->
        case mnesia:create_table(TableName, TableDef) of
            {atomic, ok} ->
                io:format("✅ Table ~p created on ~p~n", [TableName, Node]);
            {aborted, {already_exists, TableName}} ->
                io:format("✅ Table ~p already exists on ~p~n", [TableName, Node]);
            Error ->
                io:format("❌ Table ~p error: ~p~n", [TableName, Error])
        end
    end, Tables).

clear_tables() ->
    Tables = [online_users, pending_messages],
    lists:foreach(fun(Table) ->
        mnesia:clear_table(Table),
        io:format("✅ Table ~p cleared~n", [Table])
    end, Tables).