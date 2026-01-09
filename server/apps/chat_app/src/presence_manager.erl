-module(presence_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    user_online/2,
    user_offline/1,
    %% Compatibilidade: alguns pontos antigos chamam esta função
    user_offline_due_to_internet/2,
    get_user_status/1,
    is_user_online/1,
    get_all_online_users/0,
    cleanup_disconnected_users/0,
    get_last_seen/1
]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-include_lib("kernel/include/logger.hrl").

-record(state, {}).
-record(user_presence, {
    user_id :: binary(),
    ws_pid :: pid(),
    last_heartbeat :: integer(),
    is_connected :: boolean()
}).

%%%===================================================================
%%% API Pública - CORRIGIDA
%%%===================================================================

start_link() ->
    case gen_server:start_link({local, ?MODULE}, ?MODULE, [], []) of
        {ok, Pid} = Result ->
            io:format("✅ Presence Manager iniciado: ~p~n", [Pid]),
            Result;
        {error, {already_started, Pid}} = Result ->
            io:format("ℹ️  Presence Manager já estava rodando: ~p~n", [Pid]),
            Result;
        Error ->
            io:format("❌ ERRO ao iniciar Presence Manager: ~p~n", [Error]),
            Error
    end.

%% ✅ Usuário conectou (app aberto + internet + websocket)
user_online(UserId, WsPid) ->
    try
        gen_server:cast(?MODULE, {user_online, UserId, WsPid})
    catch
        exit:{noproc, _} ->
            io:format("❌ ERRO: Presence Manager não está rodando ao tentar registrar usuário online!~n"),
            ok
    end.

%% ✅ Usuário desconectou (fechou app ou perdeu internet)
user_offline(UserId) ->
    try
        gen_server:cast(?MODULE, {user_offline, UserId})
    catch
        exit:{noproc, _} ->
            io:format("❌ ERRO: Presence Manager não está rodando ao tentar registrar usuário offline!~n"),
            ok
    end.

%% ✅ Compat: tratar desconexão por internet como offline normal
user_offline_due_to_internet(UserId, _Timestamp) ->
    user_offline(UserId).

%% ✅ Verificar se usuário está ONLINE (apenas se conectado agora)
get_user_status(UserId) ->
    try
        gen_server:call(?MODULE, {get_user_status, UserId}, 5000)
    catch
        exit:{noproc, _} ->
            ?LOG_ERROR("❌ Presence Manager não está rodando!"),
            {error, presence_manager_not_running};
        exit:{timeout, _} ->
            ?LOG_ERROR("❌ Timeout ao chamar Presence Manager"),
            {error, timeout};
        Error:Reason ->
            ?LOG_ERROR("❌ Erro ao chamar Presence Manager: ~p:~p", [Error, Reason]),
            {error, {Error, Reason}}
    end.

%% ✅ Verificar se usuário está online (boolean)
is_user_online(UserId) ->
    gen_server:call(?MODULE, {is_user_online, UserId}).

%% ✅ Obter TODOS os usuários online no momento
get_all_online_users() ->
    gen_server:call(?MODULE, get_all_online_users).

%% ✅ Limpar usuários desconectados
cleanup_disconnected_users() ->
    gen_server:cast(?MODULE, cleanup_disconnected_users).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Inicializar tabela ETS para presença
    case ets:info(user_presence) of
        undefined ->
            ets:new(user_presence, [named_table, public, {keypos, 2}]);
        _ ->
            io:format("ℹ️  Tabela user_presence já existe~n")
    end,
    
    io:format("✅ Presence Manager inicializado com sucesso~n"),
    
    %% ✅ INICIAR CLEANUP AUTOMÁTICO (a cada 30 segundos)
    erlang:send_after(30000, self(), cleanup),
    
    {ok, #state{}}.

handle_call({get_user_status, UserId}, _From, State) ->
    case ets:lookup(user_presence, UserId) of
        [#user_presence{is_connected = true, ws_pid = WsPid}] ->
            %% ✅ VERIFICAÇÃO MELHORADA: Priorizar WebSocket vivo sobre heartbeat
            case WsPid of
                undefined ->
                    %% Sem WebSocket - offline
                    case get_last_seen_internal(UserId) of
                        {ok, LastSeen} ->
                            {reply, {ok, offline, LastSeen}, State};
                        _ ->
                            {reply, {ok, offline, null}, State}
                    end;
                Pid when is_pid(Pid) ->
                    case is_process_alive(Pid) of
                        true ->
                            %% ✅ WebSocket está vivo - considerar ONLINE mesmo com heartbeat antigo
                            %% (app pode estar em background/minimizada)
                            io:format("   📱 WebSocket vivo para ~p - considerando ONLINE (app可能在background)~n", [UserId]),
                            {reply, {ok, online, null}, State};
                        false ->
                            %% WebSocket morto - offline com last_seen
                            case get_last_seen_internal(UserId) of
                                {ok, LastSeen} ->
                                    {reply, {ok, offline, LastSeen}, State};
                                _ ->
                                    {reply, {ok, offline, null}, State}
                            end
                    end
            end;
        _ ->
            %% Usuário offline - buscar last_seen do banco
            case get_last_seen_internal(UserId) of
                {ok, LastSeen} ->
                    {reply, {ok, offline, LastSeen}, State};
                _ ->
                    {reply, {ok, offline, null}, State}
            end
    end;

handle_call({is_user_online, UserId}, _From, State) ->
    case ets:lookup(user_presence, UserId) of
        [#user_presence{is_connected = true}] ->
            {reply, {ok, true}, State};
        _ ->
            {reply, {ok, false}, State}
    end;

handle_call(get_all_online_users, _From, State) ->
    Now = erlang:system_time(second),
    
    %% ✅ APENAS usuários com conexão ATIVA nos últimos 30 segundos
    OnlineUsers = ets:match_object(
        user_presence, 
        #user_presence{is_connected = true, _ = '_'}
    ),
    
    %% Filtrar por heartbeat recente (max 30 segundos)
    ActiveUsers = lists:filter(
        fun(#user_presence{last_heartbeat = Heartbeat}) ->
            (Now - Heartbeat) =< 30
        end,
        OnlineUsers
    ),
    
    UserIds = [UserId || #user_presence{user_id = UserId} <- ActiveUsers],
    {reply, {ok, UserIds}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({user_online, UserId, WsPid}, State) ->
    Now = erlang:system_time(second),
    
    %% Verificar se é uma NOVA conexão (mudança de offline para online)
    WasOnline = case ets:lookup(user_presence, UserId) of
        [#user_presence{is_connected = true}] -> true;
        _ -> false
    end,
    
    %% ✅ ATUALIZAR como CONECTADO com timestamp de "visto recentemente"
    ets:insert(user_presence, #user_presence{
        user_id = UserId,
        ws_pid = WsPid,
        last_heartbeat = Now,
        is_connected = true
    }),
    
    io:format("✅✅✅ Usuário ~p ficou ONLINE (app aberto + internet)~n", [UserId]),
    
    %% ✅ BROADCAST APENAS se mudou de status
    case not WasOnline of
        true ->
            broadcast_presence_change(UserId, online, Now);
        false ->
            ok
    end,
    
    {noreply, State};

handle_cast({user_offline, UserId}, State) ->
    %% FORÇAR offline imediato com timestamp atual para grace period
    Now = erlang:system_time(second),

    ets:insert(user_presence, #user_presence{
        user_id = UserId,
        ws_pid = undefined,
        last_heartbeat = Now,  % Timestamp atual para grace period
        is_connected = false
    }),

    save_last_seen(UserId, Now),
    io:format("🔌🔌🔌 Usuário ~p ficou OFFLINE (forçado) - Grace period iniciado~n", [UserId]),
    broadcast_presence_change(UserId, offline, Now),

    {noreply, State};

handle_cast({user_offline_due_to_internet, UserId, _Ts}, State) ->
    %% Reutiliza a mesma lógica de user_offline (forçado)
    gen_server:cast(self(), {user_offline, UserId}),
    {noreply, State};

handle_cast(cleanup_disconnected_users, State) ->
    Now = erlang:system_time(second),
    
    %% ✅ LIMPAR usuários com heartbeat muito antigo (> 90 segundos) E WebSocket desconectado
    AllUsers = ets:match_object(user_presence, #user_presence{_ = '_'}),
    
    lists:foreach(
        fun(#user_presence{user_id = UserId, last_heartbeat = Heartbeat, is_connected = Connected, ws_pid = WsPid}) ->
            case (Now - Heartbeat) > 90 of
                true when Connected ->
                    %% Verificar se o WebSocket ainda está vivo
                    case WsPid of
                        undefined ->
                            %% Não tem WebSocket - marcar como offline
                            io:format("🧹 Cleanup: Usuário ~p sem WebSocket - marcando offline~n", [UserId]),
                            ets:insert(user_presence, #user_presence{
                                user_id = UserId,
                                ws_pid = undefined,
                                last_heartbeat = Now,
                                is_connected = false
                            }),
                            save_last_seen(UserId, Now),
                            broadcast_presence_change(UserId, offline, Now);
                        Pid when is_pid(Pid) ->
                            case is_process_alive(Pid) of
                                false ->
                                    %% WebSocket morreu - marcar como offline
                                    io:format("🧹 Cleanup: Usuário ~p - WebSocket morto, marcando offline~n", [UserId]),
                                    ets:insert(user_presence, #user_presence{
                                        user_id = UserId,
                                        ws_pid = undefined,
                                        last_heartbeat = Now,
                                        is_connected = false
                                    }),
                                    save_last_seen(UserId, Now),
                                    broadcast_presence_change(UserId, offline, Now);
                                true ->
                                    %% WebSocket ainda vivo mas sem heartbeat - pode estar em background
                                    %% NÃO marcar como offline, apenas log
                                    io:format("ℹ️  Usuário ~p: WebSocket ativo mas sem heartbeat (background?)~n", [UserId])
                            end
                    end;
                true when not Connected ->
                    %% Remover usuários offline antigos da tabela (> 1 hora)
                    case (Now - Heartbeat) > 3600 of
                        true ->
                            ets:delete(user_presence, UserId);
                        false ->
                            ok
                    end;
                false ->
                    ok
            end
        end,
        AllUsers
    ),
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% ✅ EXECUTAR CLEANUP e reprogramar
    gen_server:cast(?MODULE, cleanup_disconnected_users),
    erlang:send_after(30000, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

%%%===================================================================
%%% Funções Internas
%%%===================================================================

broadcast_presence_change(UserId, Status, Timestamp) ->
    PresenceMsg = #{
        <<"type">> => <<"presence">>,
        <<"user_id">> => UserId,
        <<"status">> => Status,
        <<"timestamp">> => Timestamp
    },
    
    io:format("📡 Broadcast presença ~p para contatos de ~p~n", [Status, UserId]),
    
    %% ✅ OBTER USUÁRIOS ONLINE DIRETAMENTE DA TABELA ETS (sem timeout de heartbeat)
    %% Importante: aqui queremos avisar TODOS os contatos com WebSocket conectado,
    %% mesmo que o último heartbeat esteja um pouco antigo.
    OnlineEntries = ets:match_object(
        user_presence,
        #user_presence{is_connected = true, _ = '_'}
    ),
    OnlineUserIds = [UId || #user_presence{user_id = UId} <- OnlineEntries],
    
    %% ✅ BUSCAR CONTATOS E ENVIAR PARA ELES
    case get_contacts(UserId) of
        {ok, Contacts} ->
            RelevantContacts = lists:filter(
                fun(ContactId) -> 
                    lists:member(ContactId, Contacts) 
                end, 
                OnlineUserIds
            ),
            
            lists:foreach(
                fun(ContactId) ->
                    case ets:lookup(user_presence, ContactId) of
                        [#user_presence{ws_pid = WsPid}] when is_pid(WsPid) ->
                            WsPid ! {send_message, PresenceMsg},
                            io:format("   📤 Enviado presença para ~p~n", [ContactId]);
                        _ ->
                            ok
                    end
                end,
                RelevantContacts
            );
        _ ->
            io:format("   ⚠️ Não foi possível obter contatos para broadcast~n")
    end.

%% ✅ Buscar contatos baseado em mensagens trocadas
get_contacts(UserId) ->
    try
        UserIdInt = binary_to_integer_wrapper(UserId),
        
        db_pool:with_connection(fun(Conn) ->
            %% Buscar todos os usuários com quem houve troca de mensagens
            Query = "
                SELECT DISTINCT 
                    CASE 
                        WHEN sender_id = $1 THEN receiver_id
                        ELSE sender_id
                    END as contact_id
                FROM messages
                WHERE sender_id = $1 OR receiver_id = $1
                ORDER BY contact_id
            ",
            
            case epgsql:equery(Conn, Query, [UserIdInt]) of
                {ok, _, Rows} ->
                    ContactIds = lists:map(
                        fun({ContactIdInt}) ->
                            erlang:integer_to_binary(ContactIdInt)
                        end,
                        Rows
                    ),
                    {ok, ContactIds};
                {error, Error} ->
                    io:format("❌ Erro ao buscar contatos: ~p~n", [Error]),
                    {ok, []}
            end
        end)
    catch
        Error:Reason ->
            io:format("❌ Erro na busca de contatos: ~p:~p~n", [Error, Reason]),
            {ok, []}
    end.

binary_to_integer_wrapper(Binary) when is_binary(Binary) ->
    list_to_integer(binary_to_list(Binary));
binary_to_integer_wrapper(Integer) when is_integer(Integer) ->
    Integer.

%% ✅ Salvar última vez online no banco de dados
save_last_seen(UserId, _Timestamp) ->
    try
        UserIdInt = binary_to_integer_wrapper(UserId),
        
        db_pool:with_connection(fun(Conn) ->
            %% Usar NOW() do PostgreSQL para timestamp correto
            Sql = "UPDATE users SET last_seen = NOW() WHERE id = $1",
            case epgsql:equery(Conn, Sql, [UserIdInt]) of
                {ok, _} ->
                    ?LOG_INFO("✅ Last seen salvo para usuário ~p", [UserId]);
                {error, Error} ->
                    ?LOG_ERROR("❌ Erro ao salvar last_seen: ~p", [Error])
            end
        end)
    catch
        Error:Reason ->
            ?LOG_ERROR("❌ Erro ao salvar last_seen: ~p:~p", [Error, Reason])
    end.

%% ✅ Obter última vez online de um usuário (pública)
get_last_seen(UserId) ->
    get_last_seen_internal(UserId).

%% ✅ Obter última vez online de um usuário (interna)
get_last_seen_internal(UserId) ->
    try
        UserIdInt = binary_to_integer_wrapper(UserId),
        
        db_pool:with_connection(fun(Conn) ->
            Sql = "SELECT last_seen FROM users WHERE id = $1",
            case epgsql:equery(Conn, Sql, [UserIdInt]) of
                {ok, _, [{LastSeen}]} when LastSeen =/= null ->
                    {ok, LastSeen};
                {ok, _, [{null}]} ->
                    {ok, null};
                {ok, _, []} ->
                    {ok, null};
                {error, Error} ->
                    ?LOG_ERROR("❌ Erro ao buscar last_seen: ~p", [Error]),
                    {ok, null}  % Retornar null em caso de erro
            end
        end)
    catch
        Error:Reason ->
            ?LOG_ERROR("❌ Erro na busca de last_seen: ~p:~p", [Error, Reason]),
            {ok, null}  % Retornar null em caso de erro
    end.