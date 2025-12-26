-module(user_session).
-behaviour(gen_server).

-export([start_link/1, user_online/2, user_offline/1, get_status/1, send_message/3, get_online_users/0]).
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
    {noreply, State#state{ws_pid = undefined, status = offline}}.

%% REMOVIDO ANTIGO HANDLE_CAST SEND_MESSAGE POIS AGORA É CALL

handle_call({send_message, FromId, Message}, _From, State = #state{ws_pid = WsPid, status = online}) ->
    io:format("🎯🎯🎯 DEBUG USER_SESSION (CALL) 🎯🎯🎯~n", []),
    io:format("   FromId: ~p~n", [FromId]),
    io:format("   State UserId: ~p~n", [State#state.user_id]),
    
    %% Adicionar remetente à mensagem se não existir
    EnhancedMessage = case maps:get(<<"from">>, Message, undefined) of
        undefined -> Message#{<<"from">> => FromId};
        _ -> Message
    end,
    
    WsPid ! {send_message, EnhancedMessage},
    io:format("📤 Enviando mensagem de ~p para ~p~n", [FromId, State#state.user_id]),
    {reply, ok, State};

handle_call({send_message, _FromId, _Message}, _From, State = #state{status = offline}) ->
    io:format("💾 Usuário ~p offline (status=offline) - retornando erro~n", [State#state.user_id]),
    {reply, {error, user_offline}, State};

handle_call(get_status, _From, State) ->
    {reply, {ok, State#state.status}, State}.

handle_info(_Info, State) ->
    {noreply, State}.

%% Funções auxiliares
get_process_name(UserId) when is_binary(UserId) ->
    get_process_name(binary_to_list(UserId));
get_process_name(UserId) ->
    list_to_atom("user_session_" ++ UserId).