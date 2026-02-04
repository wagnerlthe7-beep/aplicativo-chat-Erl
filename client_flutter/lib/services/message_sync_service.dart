// services/message_sync_service.dart
// Serviço de sincronização automática de mensagens pendentes

import 'dart:async';
import 'pending_messages_storage.dart';
import '../chat_service.dart';
import '../message_operations_service.dart';
import 'package:connectivity_plus/connectivity_plus.dart';

class MessageSyncService {
  static Timer? _syncTimer; // ✅ Mantido para compatibilidade com stop()
  static StreamSubscription? _connectivitySubscription;
  static bool _isSyncing = false;

  // ✅ Inicializar serviço de sincronização
  // ✅ REMOVIDO: Polling periódico - sincronização apenas em eventos reais
  static Future<void> initialize() async {
    print('🚀 Iniciando MessageSyncService...');

    // ✅ Escutar mudanças de conectividade
    _connectivitySubscription = Connectivity().onConnectivityChanged.listen((
      result,
    ) {
      if (result != ConnectivityResult.none) {
        print('🌐 Conectividade detectada -> Iniciando sincronização...');
        syncPendingMessages();
      } else {
        print('❌ Sem conectividade -> Pausando sincronização');
      }
    });

    // ✅ REMOVIDO: _startPeriodicSync() - não fazer polling periódico
    // A sincronização agora acontece apenas em eventos reais:
    // - Reconexão de WebSocket
    // - Mudança de conectividade
    // - Envio falhou explicitamente
    // - App volta do background

    // ✅ Sincronizar imediatamente se houver conectividade (apenas uma vez na inicialização)
    final connectivityResult = await Connectivity().checkConnectivity();
    if (connectivityResult != ConnectivityResult.none) {
      syncPendingMessages();
    }
  }

  // ✅ REMOVIDO: _startPeriodicSync() - polling periódico foi removido

  // ✅ Parar serviço de sincronização
  static void stop() {
    _syncTimer?.cancel();
    _syncTimer = null;
    _connectivitySubscription?.cancel();
    _connectivitySubscription = null;
    print('🛑 MessageSyncService parado');
  }

  // ✅ Sincronizar mensagens pendentes
  static Future<void> syncPendingMessages() async {
    // ✅ Evitar múltiplas sincronizações simultâneas
    if (_isSyncing) {
      print('⏳ Sincronização já em progresso, ignorando...');
      return;
    }

    // ✅ VERIFICAR INTERNET REAL (não apenas conectividade de rede)
    // Connectivity pode reportar wifi/mobile mesmo sem internet (modo avião)
    final status = await ChatService.checkConnectionStatus();
    if (status == 'no_internet') {
      print('❌ No Internet Connection -> Não sincronizando');
      return;
    } else if (status == 'server_unavailable') {
      print('❌ Server Unavailable -> Não sincronizando');
      return;
    }

    // ✅ Verificar se há conexão (internet + servidor)
    if (ChatService.isServerDown) {
      print('❌ Servidor offline -> Não sincronizando');
      return;
    }

    // ✅ Verificar se WebSocket está conectado
    if (!ChatService.isWebSocketConnected()) {
      print('❌ WebSocket não conectado -> Tentando conectar...');
      await ChatService.connect();
      if (!ChatService.isWebSocketConnected()) {
        print('❌ Falha ao conectar WebSocket -> Não sincronizando');
        return;
      }
    }

    _isSyncing = true;
    print('🔄 Iniciando sincronização de mensagens pendentes...');

    try {
      // ✅ Buscar todas as mensagens pending_local
      final pendingMessages =
          await PendingMessagesStorage.getPendingLocalMessages();
      print('📋 Encontradas ${pendingMessages.length} mensagens pendentes');

      if (pendingMessages.isEmpty) {
        _isSyncing = false;
        return;
      }

      // ✅ Tentar enviar cada mensagem
      final List<String> syncedMessageIds = [];

      for (final message in pendingMessages) {
        // ✅ Verificar se excedeu max retries
        if (await PendingMessagesStorage.hasExceededMaxRetries(message.msgId)) {
          print(
            '⚠️ Mensagem ${message.msgId} excedeu max retries -> Marcando como falha',
          );
          await PendingMessagesStorage.updateMessageStatus(
            message.msgId,
            'failed',
          );
          continue;
        }

        try {
          print('📤 Tentando enviar mensagem pendente: ${message.msgId}');

          // ✅ Verificar se é reply, edit ou delete
          if (message.replyToId != null) {
            // ✅ É uma reply - usar MessageOperationsService
            print('   📎 É uma reply -> usando MessageOperationsService');
            try {
              final result = await MessageOperationsService.replyToMessage(
                message.replyToId!,
                message.content,
                receiverId: message.to,
              );

              if (result['success'] == true) {
                final dbMessageId = result['reply_message']?['id']?.toString();
                if (dbMessageId != null) {
                  await PendingMessagesStorage.updateMessageStatus(
                    message.msgId,
                    result['reply_message']?['status']?.toString() ?? 'sent',
                    dbMessageId: dbMessageId,
                  );
                  syncedMessageIds.add(message.msgId);
                  print('✅ Reply ${message.msgId} sincronizada com sucesso');
                }
              }
            } catch (e) {
              print('❌ Erro ao sincronizar reply ${message.msgId}: $e');
              await PendingMessagesStorage.incrementRetryCount(message.msgId);
            }
          } else if (message.isEdited) {
            // ✅ É uma edição - usar MessageOperationsService
            print('   ✏️ É uma edição -> usando MessageOperationsService');
            try {
              // ✅ Usar dbMessageId se disponível, senão usar msgId
              final messageIdToUse = message.dbMessageId ?? message.msgId;
              final result = await MessageOperationsService.editMessage(
                messageIdToUse,
                message.content,
              );

              if (result['success'] == true) {
                // ✅ Atualizar status no sqflite
                await PendingMessagesStorage.updateMessageStatus(
                  message.msgId,
                  result['edited_message']?['status']?.toString() ?? 'sent',
                );
                syncedMessageIds.add(message.msgId);
                print('✅ Edição ${message.msgId} sincronizada com sucesso');
              }
            } catch (e) {
              print('❌ Erro ao sincronizar edição ${message.msgId}: $e');
              await PendingMessagesStorage.incrementRetryCount(message.msgId);
            }
          } else if (message.isDeleted) {
            // ✅ É uma deleção - usar MessageOperationsService
            print('   🗑️ É uma deleção -> usando MessageOperationsService');
            try {
              // ✅ Usar dbMessageId se disponível, senão usar msgId
              final messageIdToUse = message.dbMessageId ?? message.msgId;
              final result = await MessageOperationsService.deleteMessage(
                messageIdToUse,
              );

              if (result['success'] == true) {
                syncedMessageIds.add(message.msgId);
                print('✅ Deleção ${message.msgId} sincronizada com sucesso');
              }
            } catch (e) {
              print('❌ Erro ao sincronizar deleção ${message.msgId}: $e');
              await PendingMessagesStorage.incrementRetryCount(message.msgId);
            }
          } else {
            // ✅ Mensagem normal - usar ChatService
            await ChatService.sendMessage(
              message.to,
              message.content,
              tempId: message.msgId,
            );

            // ✅ Se chegou aqui, a mensagem foi enviada com sucesso
            // O ChatService vai atualizar o status quando receber confirmação do servidor
            print('✅ Mensagem ${message.msgId} enviada com sucesso');
          }
        } catch (e) {
          print('❌ Erro ao enviar mensagem ${message.msgId}: $e');

          // ✅ Incrementar retry count
          await PendingMessagesStorage.incrementRetryCount(message.msgId);
        }
      }

      // ✅ Limpar mensagens sincronizadas (status != pending_local)
      if (syncedMessageIds.isNotEmpty) {
        for (final msgId in syncedMessageIds) {
          final msg = await PendingMessagesStorage.getMessageById(msgId);
          if (msg != null && msg.status != 'pending_local') {
            await PendingMessagesStorage.deleteMessage(msgId);
            print('🧹 Mensagem sincronizada removida: $msgId');
          }
        }
      }

      print('✅ Sincronização concluída');
    } catch (e) {
      print('❌ Erro na sincronização: $e');
    } finally {
      _isSyncing = false;
    }
  }

  // ✅ Sincronizar manualmente (chamado após envio bem-sucedido)
  static Future<void> syncNow() async {
    await syncPendingMessages();
  }

  // ✅ Verificar se há mensagens pendentes
  static Future<bool> hasPendingMessages() async {
    final count = await PendingMessagesStorage.countPendingMessages(
      status: 'pending_local',
    );
    return count > 0;
  }

  // ✅ Obter contagem de mensagens pendentes
  static Future<int> getPendingCount() async {
    return await PendingMessagesStorage.countPendingMessages(
      status: 'pending_local',
    );
  }
}
