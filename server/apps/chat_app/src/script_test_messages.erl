%%%-------------------------------------------------------------------
%%% script_test_messages.erl - Script para testar persistência de mensagens
%%%-------------------------------------------------------------------
-module(script_test_messages).
-export([run_all_tests/0, check_table_structure/0, cleanup_test_messages/0]).

-include_lib("kernel/include/logger.hrl").

run_all_tests() ->
    io:format("🚀🚀🚀 INICIANDO TESTES DE PERSISTÊNCIA DE MENSAGENS 🚀🚀🚀~n~n", []),
    
    io:format("1. Verificando estrutura da tabela...~n", []),
    Result1 = check_table_structure(),
    
    io:format("~n2. Testando salvamento de mensagem...~n", []),
    Result2 = test_save_message(),
    
    io:format("~n3. Testando busca de histórico...~n", []),
    Result3 = test_get_history(),
    
    io:format("~n4. Limpando mensagens de teste...~n", []),
    Result4 = cleanup_test_messages(),
    
    io:format("~n🎉🎉🎉 TESTES CONCLUÍDOS 🎉🎉🎉~n", []),
    
    case {Result1, Result2, Result3, Result4} of
        {ok, {ok, _}, {ok, _}, ok} -> 
            io:format("✅ TODOS OS TESTES PASSARAM!~n", []),
            ok;
        _ ->
            io:format("⚠️  ALGUNS TESTES FALHARAM~n", []),
            error
    end.

%%%===================================================================
%%% TESTE DE SALVAMENTO DE MENSAGEM
%%%===================================================================

test_save_message() ->
    SenderId = <<"1">>,
    ReceiverId = <<"2">>,
    
    % Criar conteúdo com timestamp
    Timestamp = integer_to_binary(erlang:system_time(second)),
    Content = <<"Mensagem de teste - hora: ", Timestamp/binary>>,
    
    io:format("   Enviando mensagem de ~p para ~p...~n", [SenderId, ReceiverId]),
    io:format("   Conteúdo: ~p~n", [Content]),
    
    case message_repo:save_message(SenderId, ReceiverId, Content) of
        {ok, Id} ->
            io:format("   ✅ SUCESSO! Mensagem salva com ID: ~p~n", [Id]),
            {ok, Id};
        {error, Reason} ->
            io:format("   ❌ FALHA: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% TESTE DE BUSCA DE HISTÓRICO
%%%===================================================================

test_get_history() ->
    UserId = <<"1">>,
    ContactId = <<"2">>,
    
    io:format("   Buscando histórico entre ~p e ~p...~n", [UserId, ContactId]),
    
    case message_repo:get_chat_history(UserId, ContactId) of
        {ok, Messages} ->
            io:format("   ✅ ~p mensagens encontradas~n", [length(Messages)]),
            
            case Messages of
                [] ->
                    io:format("   ⚠️  Nenhuma mensagem encontrada~n", []);
                _ ->
                    io:format("   📝 Últimas 3 mensagens:~n", []),
                    NumMessages = length(Messages),
                    Start = if 
                        NumMessages > 3 -> NumMessages - 2;
                        true -> 1
                    end,
                    LastThree = lists:sublist(Messages, Start, min(3, NumMessages)),
                    lists:foreach(fun(Msg) ->
                        io:format("     - ID: ~p, Conteúdo: ~p~n", 
                            [maps:get(<<"id">>, Msg), maps:get(<<"content">>, Msg)])
                    end, LastThree)
            end,
            {ok, Messages};
        {error, Reason} ->
            io:format("   ❌ FALHA: ~p~n", [Reason]),
            {error, Reason}
    end.

%%%===================================================================
%%% FUNÇÕES AUXILIARES
%%%===================================================================

%% Função para verificar estrutura da tabela messages
check_table_structure() ->
    io:format("🔍 Verificando estrutura da tabela messages...~n", []),
    
    case db_pool:with_connection(fun(Conn) ->
        Sql = "
            SELECT column_name, data_type, is_nullable, column_default
            FROM information_schema.columns 
            WHERE table_name = 'messages' 
            ORDER BY ordinal_position
        ",
        
        case epgsql:equery(Conn, Sql, []) of
            {ok, _, Rows} ->
                io:format("   ✅ Colunas da tabela messages:~n", []),
                lists:foreach(fun({Name, Type, Nullable, Default}) ->
                    io:format("     - ~s: ~s ~s (default: ~p)~n", 
                        [Name, Type, 
                         case Nullable of 
                             <<"YES">> -> "NULL"; 
                             _ -> "NOT NULL" 
                         end,
                         Default])
                end, Rows),
                {ok, Rows};
            {error, Error} ->
                io:format("   ❌ Erro: ~p~n", [Error]),
                {error, Error}
        end
    end) of
        {ok, _} -> ok;
        {error, Error} -> 
            io:format("   ❌ Erro na conexão: ~p~n", [Error]),
            error
    end.

%% Função para limpar mensagens de teste
cleanup_test_messages() ->
    io:format("🧹 Limpando mensagens de teste...~n", []),
    
    case db_pool:with_connection(fun(Conn) ->
        Sql = "DELETE FROM messages WHERE content LIKE 'Mensagem de teste%' 
               OR content LIKE 'Mensagem via message_router%'",
        
        case epgsql:equery(Conn, Sql, []) of
            {ok, Count} ->
                io:format("   ✅ ~p mensagens de teste removidas~n", [Count]);
            {error, Error} ->
                io:format("   ❌ Erro: ~p~n", [Error]),
                {error, Error}
        end
    end) of
        {ok, Count} when is_integer(Count) -> 
            io:format("   ✅ Cleanup concluído~n", []),
            ok;
        _ -> 
            io:format("   ⚠️  Cleanup com problemas~n", []),
            error
    end.