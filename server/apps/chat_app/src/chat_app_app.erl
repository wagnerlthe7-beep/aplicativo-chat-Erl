%%%-------------------------------------------------------------------
%% @doc chat_app public API
%% @end
%%%-------------------------------------------------------------------

%% Inicializa o servidor Cowboy e registra rotas REST

-module(chat_app_app).
-behaviour(application).

%% API
-export([start/2, stop/1]).

%% ------------------------------------------------------------------
%% Application callbacks
%% ------------------------------------------------------------------

start(_StartType, _StartArgs) ->
    %% ✅ Inicializar Pool de Conexões PostgreSQL
    {ok, _PoolPid} = db_pool:start_link(),
    %% ✅ Inicializar Presence Manager
    case presence_manager:start_link() of
        {ok, PresencePid} ->
            io:format("✅ Presence Manager iniciado com sucesso: ~p~n", [PresencePid]);
        {error, {already_started, PresencePid}} ->
            io:format("ℹ️  Presence Manager já estava iniciado: ~p~n", [PresencePid]);
        {error, Reason} ->
            io:format("❌ ERRO ao iniciar Presence Manager: ~p~n", [Reason]),
            %% Tentar iniciar novamente após 1 segundo
            timer:sleep(1000),
            case presence_manager:start_link() of
                {ok, PresencePid2} ->
                    io:format("✅ Presence Manager iniciado na segunda tentativa: ~p~n", [PresencePid2]);
                Error2 ->
                    io:format("❌ FALHA CRÍTICA: Não foi possível iniciar Presence Manager: ~p~n", [Error2]),
                    throw({presence_manager_start_failed, Error2})
            end
    end,
    %% Dispatcher com rotas
    Dispatch = cowboy_router:compile([
        {'_', [
            {"/auth/firebase", firebase_handler, []},
            {"/auth/check-user", check_user_handler, []}, %% ✅✅✅ NOVO: Verificar se usuário é novo
            {"/auth/refresh", refresh_handler, []},
            {"/auth/logout", logout_handler, []},
            {"/auth/revoke-others", revoke_others_handler, []},
            {"/auth/validate-session", validate_session_handler, []},
            {"/users/lookup", users_lookup_handler, []}, 
            {"/api/messages/history/:user_id/:contact_id", message_history_handler, []},
            {"/api/messages/mark_read/:user_id/:contact_id", message_mark_read_handler, []},
            %% Rotas para operações avançadas de mensagens
            {"/api/messages/:messageId/edit", message_operations_handler, edit},
            {"/api/messages/:messageId/delete", message_operations_handler, delete},
            {"/api/messages/:messageId/reply", message_operations_handler, reply},
            {"/api/admin/messages/:messageId/recover", message_operations_handler, recover},
            {"/api/messages/:messageId/history", message_operations_handler, history},
            {"/api/users/:user_id", user_info_handler, []},
            {"/api/presence/:user_id", presence_handler, []},  % ✅ Status de presença
            %% ✅ NOVO: FCM Push Notifications (Sistema de entrega estilo WhatsApp)
            {"/api/fcm/register", fcm_token_handler, []},     % Registar token FCM
            {"/api/fcm/unregister", fcm_token_handler, []},   % Remover token FCM
            {"/api/messages/ack", message_ack_handler, []},   % Receber ACK de mensagem
            {"/ws", ws_handler, []}  % Permite qualquer path após /ws   %% WebSocket handler
        ]}
    ]),

    %% Configura o listener HTTP
    case cowboy:start_clear(
        http_listener,           %% nome do listener
        [{port, 4000}, {ip, {0,0,0,0}}],         %% porta e IP
        #{env => #{dispatch => Dispatch}}  %% configuração do dispatcher
    ) of
        {ok, _Pid} ->
            io:format("🚀 Chat_app server listening on port 4000~n", []),
            {ok, self()};
        {error, eaddrinuse} ->
            io:format("❌ ERRO: Porta 4000 já está em uso!~n"),
            io:format("   Pare o servidor anterior ou mate o processo que está usando a porta.~n"),
            io:format("   No Windows PowerShell: netstat -ano | findstr :4000~n"),
            io:format("   Depois: taskkill /PID <PID> /F~n"),
            {error, port_in_use};
        {error, OtherReason} ->
            io:format("❌ ERRO ao iniciar servidor: ~p~n", [OtherReason]),
            {error, OtherReason}
    end.

stop(_State) ->
    ok.

