-module(user_session).
-behaviour(gen_server).

-export([
    start_link/1,
    user_online/2,
    user_offline/1,
    user_offline/2,
    get_status/1,
    send_message/3,
    get_online_users/0,
    is_websocket_alive/1
]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-record(state, {
    user_id :: binary(),
    ws_pid :: pid() | undefined,
    status = offline :: online | offline,
    last_seen :: integer()
}).

%% API Pública
start_link(UserId) ->
    gen_server:start_link(?MODULE, [UserId], []).

user_online(UserId, WsPid) ->
    case whereis(get_process_name(UserId)) of
        undefined ->
            {ok, Pid} = start_link(UserId),
            gen_server:cast(Pid, {user_online, WsPid});
        Pid ->
            gen_server:cast(Pid, {user_online, WsPid})
    end.

user_offline(UserId) ->
    case whereis(get_process_name(UserId)) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, user_offline)
    end.

%% ✅ Offline condicional: só marca offline se o WS que caiu ainda for o WS atual
%% (evita "flapping" quando existe reconexão e o socket antigo termina depois).
user_offline(UserId, WsPid) when is_pid(WsPid) ->
    case whereis(get_process_name(UserId)) of
        undefined -> ok;
        Pid -> gen_server:cast(Pid, {user_offline, WsPid})
    end.

get_status(UserId) ->
    case whereis(get_process_name(UserId)) of
        undefined -> {error, user_not_found};
        Pid -> gen_server:call(Pid, get_status)
    end.

send_message(FromId, ToId, Message) ->
    case whereis(get_process_name(ToId)) of
        undefined -> {error, user_offline};
        Pid -> 
            try gen_server:call(Pid, {send_message, FromId, Message}, 5000) of
                Result -> Result
            catch
                exit:{timeout, _} -> {error, timeout};
                exit:{noproc, _} -> {error, user_offline}
            end
    end.

is_websocket_alive(UserId) ->
    case whereis(get_process_name(UserId)) of
        undefined -> false;
        Pid ->
            try gen_server:call(Pid, get_websocket_pid, 3000) of
                WsPid when is_pid(WsPid) -> is_process_alive(WsPid);
                _ -> false
            catch
                _ -> false
            end
    end.

%% Retorna PIDs de todos usuários online
get_online_users() ->
    Pattern = {{n, l, {user_session, '$1'}}, '_', '_'},
    MatchHead = Pattern,
    Guard = [],
    Result = ['$1'],

    gproc:select([{MatchHead, Guard, Result}]).

%% Callbacks do gen_server
init([UserId]) ->
    ProcessName = get_process_name(UserId),
    register(ProcessName, self()),
    %% REGISTRO NO GPROC
    gproc:reg({n, l, {user_session, UserId}}),
    {ok, #state{user_id = UserId}}.

handle_cast({user_online, WsPid}, State) ->
    Now = erlang:system_time(second),
    io:format("✅ User ~p is now online~n", [State#state.user_id]),
    {noreply, State#state{ws_pid = WsPid, status = online, last_seen = Now}};

handle_cast(user_offline, State) ->
    io:format("🔌 User ~p is now offline~n", [State#state.user_id]),
    {noreply, State#state{ws_pid = undefined, status = offline}};

handle_cast({user_offline, WsPid}, State = #state{ws_pid = CurrentWsPid}) ->
    case CurrentWsPid of
        WsPid ->
            io:format("🔌 User ~p is now offline (ws_pid match)~n", [State#state.user_id]),
            {noreply, State#state{ws_pid = undefined, status = offline}};
        _Other ->
            io:format("ℹ️  Ignorando offline para ~p: ws_pid antigo ~p (atual=~p)~n",
                      [State#state.user_id, WsPid, CurrentWsPid]),
            {noreply, State}
    end.

handle_call({send_message, FromId, Message}, _From, State = #state{ws_pid = WsPid, status = online}) ->
    io:format("🎯🎯🎯 DEBUG USER_SESSION (CALL) 🎯🎯🎯~n", []),
    io:format("   FromId: ~p~n", [FromId]),
    io:format("   State UserId: ~p~n", [State#state.user_id]),
    io:format("   WebSocket PID: ~p~n", [WsPid]),
    io:format("   WebSocket Alive: ~p~n", [is_process_alive(WsPid)]),
    
    %% Adicionar remetente à mensagem se não existir
    EnhancedMessage = case maps:get(<<"from">>, Message, undefined) of
        undefined -> Message#{<<"from">> => FromId};
        _ -> Message
    end,
    
    %% VERIFICAÇÃO CRÍTICA: WebSocket ainda está vivo?
    case is_process_alive(WsPid) of
        true ->
            WsPid ! {send_message, EnhancedMessage},
            io:format("📤 Enviando mensagem de ~p para ~p (WS vivo)~n", [FromId, State#state.user_id]),
            {reply, ok, State};
        false ->
            io:format("❌ WebSocket morto para usuário ~p - retornando erro~n", [State#state.user_id]),
            {reply, {error, user_offline}, State}
    end;

handle_call({send_message, _FromId, _Message}, _From, State = #state{status = offline}) ->
    io:format(" Usuário ~p offline (status=offline) - retornando erro~n", [State#state.user_id]),
    {reply, {error, user_offline}, State};

handle_call(get_status, _From, State) ->
    {reply, {ok, State#state.status}, State};

handle_call(get_websocket_pid, _From, State) ->
    {reply, State#state.ws_pid, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Funções auxiliares
get_process_name(UserId) when is_binary(UserId) ->
    get_process_name(binary_to_list(UserId));
get_process_name(UserId) ->
    list_to_atom("user_session_" ++ UserId).