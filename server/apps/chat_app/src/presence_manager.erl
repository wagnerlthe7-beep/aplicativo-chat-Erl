-module(presence_manager).
-behaviour(gen_server).

-export([
    start_link/0,
    user_online/2,
    user_offline/1,
    user_offline/2,
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

%% ✅ Offline condicional: só marca offline se o WS que caiu ainda for o WS atual
%% (evita alternância online/offline com reconexões rápidas).
user_offline(UserId, WsPid) when is_pid(WsPid) ->
    try
        gen_server:cast(?MODULE, {user_offline, UserId, WsPid})
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
    
    %% ✅ INICIAR CLEANUP AUTOMÁTICO (a cada 3 segundos para detecção rápida de 6s)
    erlang:send_after(3000, self(), cleanup),
    
    {ok, #state{}}.

handle_call({get_user_status, UserId}, _From, State) ->
    case ets:lookup(user_presence, UserId) of
        [#user_presence{is_connected = true, ws_pid = WsPid, last_heartbeat = LastHeartbeat}] ->
            Now = erlang:system_time(second),
            HeartbeatAge = Now - LastHeartbeat,
            
            %% ✅ VERIFICAÇÃO CRÍTICA: WebSocket deve estar vivo E heartbeat recente (< 6 segundos)
            %% Se heartbeat está muito antigo (> 6s), usuário está offline mesmo que processo exista
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
                        true when HeartbeatAge =< 6 ->
                            %% ✅ WebSocket está vivo E heartbeat recente - REALMENTE online
                            io:format("   📱 Usuário ~p REALMENTE online (WS vivo, heartbeat há ~p segundos)~n", [UserId, HeartbeatAge]),
                            {reply, {ok, online, null}, State};
                        true when HeartbeatAge > 6 ->
                            %% ⚠️ WebSocket vivo mas heartbeat muito antigo - provavelmente offline (sem internet)
                            io:format("   ⚠️ Usuário ~p com WS vivo mas heartbeat antigo (~p segundos) - marcando OFFLINE~n", [UserId, HeartbeatAge]),
                            %% Marcar como offline imediatamente
                            ets:insert(user_presence, #user_presence{
                                user_id = UserId,
                                ws_pid = undefined,
                                last_heartbeat = Now,
                                is_connected = false
                            }),
                            save_last_seen(UserId, Now),
                            broadcast_presence_change(UserId, offline, Now),
                            case get_last_seen_internal(UserId) of
                                {ok, LastSeen} ->
                                    {reply, {ok, offline, LastSeen}, State};
                                _ ->
                                    {reply, {ok, offline, null}, State}
                            end;
                        false ->
                            %% WebSocket morto - offline com last_seen
                            io:format("   🔌 WebSocket morto para ~p - marcando OFFLINE~n", [UserId]),
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
        [#user_presence{is_connected = true, ws_pid = WsPid, last_heartbeat = LastHeartbeat}] ->
            Now = erlang:system_time(second),
            HeartbeatAge = Now - LastHeartbeat,
            
            %% ✅ VERIFICAÇÃO CRÍTICA: WebSocket deve estar vivo E heartbeat recente
            case WsPid of
                undefined ->
                    {reply, {ok, false}, State};
                Pid when is_pid(Pid) ->
                    case is_process_alive(Pid) andalso HeartbeatAge =< 6 of
                        true ->
                            {reply, {ok, true}, State};
                        false ->
                            %% WebSocket morto ou heartbeat muito antigo - offline
                            {reply, {ok, false}, State}
                    end
            end;
        _ ->
            {reply, {ok, false}, State}
    end;

handle_call(get_all_online_users, _From, State) ->
    Now = erlang:system_time(second),
    
    %% ✅ APENAS usuários com conexão ATIVA nos últimos 60 segundos E WebSocket vivo
    OnlineUsers = ets:match_object(
        user_presence, 
        #user_presence{is_connected = true, _ = '_'}
    ),
    
    %% Filtrar por heartbeat recente (max 6 segundos) E WebSocket vivo
    ActiveUsers = lists:filter(
        fun(#user_presence{last_heartbeat = Heartbeat, ws_pid = WsPid}) ->
            HeartbeatAge = Now - Heartbeat,
            IsRecent = HeartbeatAge =< 6,
            IsWsAlive = case WsPid of
                undefined -> false;
                Pid when is_pid(Pid) -> is_process_alive(Pid);
                _ -> false
            end,
            IsRecent andalso IsWsAlive
        end,
        OnlineUsers
    ),
    
    UserIds = [UserId || #user_presence{user_id = UserId} <- ActiveUsers],
    {reply, {ok, UserIds}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({user_online, UserId, WsPid}, State) ->
    Now = erlang:system_time(second),
    
    %% ✅ Verificar estado anterior ANTES de atualizar
    PreviousState = ets:lookup(user_presence, UserId),
    ShouldBroadcast = case PreviousState of
        [#user_presence{is_connected = true, last_heartbeat = LastHeartbeat, ws_pid = PrevPid}] ->
            %% ✅ Verificar se heartbeat está muito antigo (> 3s) - fazer broadcast para sincronizar
            %% ✅ Se o WS mudou, forçar broadcast (reconexão real)
            HeartbeatAge = Now - LastHeartbeat,
            case PrevPid =:= WsPid of
                true ->
                    if HeartbeatAge > 3 -> true;  % Heartbeat antigo, fazer broadcast para sincronizar
                    true -> false  % Heartbeat recente, não precisa broadcast
                    end;
                false ->
                    true
            end;
        [#user_presence{is_connected = false}] ->
            true;
        _ -> true  % Não estava online, fazer broadcast
    end,
    
    %% ✅ ATUALIZAR como CONECTADO com timestamp de "visto recentemente"
    ets:insert(user_presence, #user_presence{
        user_id = UserId,
        ws_pid = WsPid,
        last_heartbeat = Now,
        is_connected = true
    }),
    
    io:format("✅✅✅ Usuário ~p ficou ONLINE (app aberto + internet)~n", [UserId]),
    
    %% ✅ BROADCAST se mudou de status OU se heartbeat estava antigo (reconexão)
    if ShouldBroadcast ->
        io:format("📡 Fazendo broadcast de presença ONLINE para contatos de ~p~n", [UserId]),
        broadcast_presence_change(UserId, online, Now),
        
        %% ✅ NOVO: Enviar presença dos usuários já online para o novo usuário
        send_existing_presence_to_new_user(UserId, Now);
    true ->
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

handle_cast({user_offline, UserId, WsPid}, State) ->
    %% Offline condicional: só executar se o WsPid ainda for o WsPid atual em ETS
    case ets:lookup(user_presence, UserId) of
        [#user_presence{ws_pid = CurrentWsPid, is_connected = true}] when CurrentWsPid =:= WsPid ->
            gen_server:cast(self(), {user_offline, UserId}),
            {noreply, State};
        [#user_presence{ws_pid = CurrentWsPid, is_connected = true}] ->
            io:format("ℹ️  Ignorando offline para ~p: ws_pid antigo ~p (atual=~p)~n",
                      [UserId, WsPid, CurrentWsPid]),
            {noreply, State};
        _ ->
            %% Já está offline ou não existe entrada - nada a fazer
            {noreply, State}
    end;

handle_cast({user_offline_due_to_internet, UserId, _Ts}, State) ->
    %% Reutiliza a mesma lógica de user_offline (forçado)
    gen_server:cast(self(), {user_offline, UserId}),
    {noreply, State};

handle_cast(cleanup_disconnected_users, State) ->
    Now = erlang:system_time(second),
    
    %% ✅ LIMPAR usuários com heartbeat muito antigo (> 6 segundos) - marcar como offline IMEDIATAMENTE
    AllUsers = ets:match_object(user_presence, #user_presence{_ = '_'}),
    
    lists:foreach(
        fun(#user_presence{user_id = UserId, last_heartbeat = Heartbeat, is_connected = Connected, ws_pid = WsPid}) ->
            HeartbeatAge = Now - Heartbeat,
            case HeartbeatAge > 6 of
                true when Connected ->
                    %% ✅ Heartbeat muito antigo (> 6s) - usuário está offline (sem internet)
                    %% Marcar como offline IMEDIATAMENTE, independente de WebSocket
                    io:format("🧹 Cleanup: Usuário ~p com heartbeat antigo (~p segundos) - marcando OFFLINE~n", [UserId, HeartbeatAge]),
                    ets:insert(user_presence, #user_presence{
                        user_id = UserId,
                        ws_pid = undefined,
                        last_heartbeat = Now,
                        is_connected = false
                    }),
                    save_last_seen(UserId, Now),
                    broadcast_presence_change(UserId, offline, Now);
                true when not Connected ->
                    %% Já está offline - verificar se deve remover da tabela
                    case HeartbeatAge > 3600 of
                        true ->
                            ets:delete(user_presence, UserId);
                        false ->
                            ok
                    end;
                false ->
                    %% Heartbeat recente - verificar se WebSocket ainda está vivo
                    case Connected of
                        true ->
                            case WsPid of
                                undefined ->
                                    %% Sem WebSocket - marcar como offline
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
                                            %% WebSocket vivo e heartbeat recente - OK
                                            ok
                                    end
                            end;
                        false ->
                            ok
                    end
            end
        end,
        AllUsers
    ),
    
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(cleanup, State) ->
    %% ✅ EXECUTAR CLEANUP e reprogramar (a cada 3 segundos para detecção rápida de 6s)
    gen_server:cast(?MODULE, cleanup_disconnected_users),
    erlang:send_after(3000, self(), cleanup),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.
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
    
    io:format("   📋 Usuários online: ~p~n", [OnlineUserIds]),
    
    %% ✅ BUSCAR CONTATOS E ENVIAR PARA ELES
    case get_contacts(UserId) of
        {ok, Contacts} ->
            io:format("   📋 Contatos de ~p: ~p~n", [UserId, Contacts]),
            
            RelevantContacts = lists:filter(
                fun(ContactId) -> 
                    lists:member(ContactId, Contacts) 
                end, 
                OnlineUserIds
            ),
            
            io:format("   🎯 Contatos relevantes (online + contatos): ~p~n", [RelevantContacts]),
            
            lists:foreach(
                fun(ContactId) ->
                    io:format("   📤 Processando envio para ~p...~n", [ContactId]),
                    %% ✅ USAR user_session:send_message para garantir entrega correta
                    case user_session:send_message(UserId, ContactId, PresenceMsg) of
                        ok ->
                            io:format("   ✅ Presença enviada para ~p via user_session~n", [ContactId]);
                        {error, Reason} ->
                            io:format("   ❌ Erro ao enviar presença para ~p: ~p~n", [ContactId, Reason])
                    end
                end,
                RelevantContacts
            );
        _ ->
            io:format("   ⚠️ Não foi possível obter contatos para broadcast~n")
    end.

%% ✅ Enviar presença dos usuários já online para o novo usuário
send_existing_presence_to_new_user(NewUserId, Now) ->
    io:format("🔄 Enviando presença dos usuários já online para ~p~n", [NewUserId]),
    
    %% Obter todos os usuários online (exceto o novo usuário)
    OnlineEntries = ets:match_object(
        user_presence,
        #user_presence{is_connected = true, _ = '_'}
    ),
    OnlineUsers = [UId || #user_presence{user_id = UId} <- OnlineEntries, UId =/= NewUserId],
    
    io:format("   📋 Usuários já online para enviar para ~p: ~p~n", [NewUserId, OnlineUsers]),
    
    %% Obter contatos do novo usuário
    case get_contacts(NewUserId) of
        {ok, Contacts} ->
            %% Filtrar apenas os contatos que estão online
            RelevantOnlineUsers = lists:filter(
                fun(OnlineUserId) ->
                    lists:member(OnlineUserId, Contacts)
                end,
                OnlineUsers
            ),
            
            io:format("   🎯 Contatos online relevantes para ~p: ~p~n", [NewUserId, RelevantOnlineUsers]),
            
            %% Enviar presença de cada contato online para o novo usuário
            lists:foreach(
                fun(OnlineUserId) ->
                    PresenceMsg = #{
                        <<"type">> => <<"presence">>,
                        <<"user_id">> => OnlineUserId,
                        <<"status">> => <<"online">>,
                        <<"timestamp">> => Now
                    },
                    
                    %% ✅ USAR user_session:send_message para garantir entrega correta
                    case user_session:send_message(OnlineUserId, NewUserId, PresenceMsg) of
                        ok ->
                            io:format("   ✅ Presença de ~p enviada para novo usuário ~p via user_session~n", [OnlineUserId, NewUserId]);
                        {error, Reason} ->
                            io:format("   ❌ Erro ao enviar presença de ~p para ~p: ~p~n", [OnlineUserId, NewUserId, Reason])
                    end
                end,
                RelevantOnlineUsers
            );
        _ ->
            io:format("   ⚠️ Não foi possível obter contatos do novo usuário ~p~n", [NewUserId])
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