// lib/chat_service.dart - VERSÃO FINAL CORRIGIDA
import 'dart:convert';
import 'dart:async';
import 'dart:typed_data';
import 'dart:io';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:http/http.dart' as http;
import 'package:uuid/uuid.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:flutter/widgets.dart';
import 'chat_model.dart';
import 'auth_service.dart';

class ChatService {
  static WebSocketChannel? _channel;
  static String? _currentUserId;
  static const _secureStorage = FlutterSecureStorage();

  static final Uuid _uuid = Uuid();
  static bool _isReconnecting = false;
  static bool _isManualDisconnect = false;
  static int _reconnectAttempts = 0;
  static const int _maxReconnectAttempts = 5;

  static final _messageController =
      StreamController<Map<String, dynamic>>.broadcast();
  static final _typingController =
      StreamController<Map<String, dynamic>>.broadcast();
  static final _presenceController =
      StreamController<Map<String, dynamic>>.broadcast();
  static final Map<String, int> _presenceTimestamps = {};
  static final Set<String> _sentMessageIds = {};

  // ✅ Controle de presença
  static Timer? _heartbeatTimer;
  static final Map<String, String> _userPresenceStatus =
      {}; // user_id -> status

  // ✅ SISTEMA DE CHATS DINÂMICO
  static final _chatListController =
      StreamController<List<ChatContact>>.broadcast();
  static final Map<String, ChatContact> _chatContacts = {};
  static String? _activeChatContactId;

  static String _generateMessageId() {
    return 'msg_${DateTime.now().millisecondsSinceEpoch}_${_uuid.v4().substring(0, 8)}';
  }

  static Future<bool> connect() async {
    _isManualDisconnect = false;
    if (_channel != null && !_isReconnecting) {
      return true;
    }

    try {
      _reconnectAttempts++;
      final authData = await _loadAuthData();
      final token = authData['token'];
      _currentUserId = authData['userId'];

      if (token == null || _currentUserId == null) {
        print('❌ No authentication data found');
        return false;
      }

      final url = 'ws://10.0.2.2:4000/ws?token=$token';
      //final url = 'ws://192.168.100.17:4000/ws?token=$token';
      _channel = WebSocketChannel.connect(Uri.parse(url));

      _channel!.stream.listen(
        _handleIncomingMessage,
        onError: (error) {
          print('❌ WebSocket error: $error');
          _handleDisconnect();
        },
        onDone: () {
          print('🔌 WebSocket disconnected');
          _handleDisconnect();
        },
      );

      _isReconnecting = false;
      // NÃO resetar tentativas aqui, apenas após receber 'welcome' ou conexão estável
      // _reconnectAttempts = 0;
      print('✅ WebSocket connected for user $_currentUserId');

      // ✅ INICIAR SISTEMA DE HEARTBEAT
      _startHeartbeat();

      // ✅ RECONSTRUIR CHATS APÓS CONECTAR
      WidgetsBinding.instance.addPostFrameCallback((_) {
        rebuildChatsFromHistory();
      });

      return true;
    } catch (e) {
      print('❌ WebSocket connection error: $e');
      _handleDisconnect();
      return false;
    }
  }

  static void _handleDisconnect() {
    _channel = null;

    if (_isManualDisconnect) {
      print('🔌 Desconexão manual - não reconectando automaticamente');
      return;
    }

    // ✅ Quando desconectar (perda de internet / WS fechado),
    // marcar TODOS os contatos locais como offline para o cliente atual.
    if (_userPresenceStatus.isNotEmpty) {
      print('📡 WS desconectado - limpando status de presença local');
      final nowTs = DateTime.now().millisecondsSinceEpoch ~/ 1000;
      final ids = _userPresenceStatus.keys.toList();
      for (final userId in ids) {
        _userPresenceStatus[userId] = 'offline';
        _presenceTimestamps[userId] = nowTs;
        _presenceController.add({
          'user_id': userId,
          'status': 'offline',
          'timestamp': nowTs,
        });
      }
    }

    if (!_isReconnecting && _reconnectAttempts < _maxReconnectAttempts) {
      _isReconnecting = true;
      final delay = Duration(seconds: _reconnectAttempts * 2);
      print('🔄 Tentando reconectar em ${delay.inSeconds} segundos...');

      Future.delayed(delay, () {
        if (_isReconnecting) {
          connect();
        }
      });
    } else if (_reconnectAttempts >= _maxReconnectAttempts) {
      print('❌ Máximo de tentativas de reconexão atingido');
      _isReconnecting = false;
    }
  }

  // ✅ Informar qual chat está atualmente aberto (para controle de unread)
  static void setActiveChat(String contactId) {
    _activeChatContactId = contactId;
    print('📂 Active chat set to: $contactId');
  }

  static void clearActiveChat(String contactId) {
    if (_activeChatContactId == contactId) {
      print('📂 Active chat cleared: $contactId');
      _activeChatContactId = null;
    }
  }

  // ✅ Getter para obter o chat ativo
  static String? get activeChatContactId => _activeChatContactId;

  static void _handleIncomingMessage(dynamic data) {
    try {
      final message = json.decode(data);
      print('📨 Received: $message');

      final messageId = message['message_id']?.toString();
      final dbMessageId = message['db_message_id'];

      // ✅ CORREÇÃO: Permitir passagem se for confirmação de envio (tem db_message_id)
      // para que a UI possa atualizar o ID temporário pelo ID do banco.
      if (messageId != null && _sentMessageIds.contains(messageId)) {
        if (dbMessageId != null) {
          print(
            '🔄 Confirmação de envio recebida (permitindo para SWAP): $messageId -> $dbMessageId',
          );
          _sentMessageIds.remove(messageId);
        } else {
          print('🔄 Ignorando mensagem duplicada (echo simples): $messageId');
          _sentMessageIds.remove(messageId);
          return;
        }
      }

      switch (message['type']) {
        case 'welcome':
          print('✅ Authenticated with chat server');
          // ✅ Conexão estabelecida com sucesso - resetar contador de tentativas
          _reconnectAttempts = 0;
          break;
        case 'message':
          _messageController.add(message);
          final shouldIncreaseUnread =
              message['should_increase_unread'] ?? true;
          _updateChatOnMessageReceived(message, shouldIncreaseUnread);
          break;
        case 'message_delivered':
          _messageController.add(message);
          break;
        case 'message_read':
          _messageController.add(message);
          break;
        case 'message_edited':
          _messageController.add(message);
          print('✏️ Mensagem editada recebida: $message');
          break;
        case 'message_deleted':
          _messageController.add(message);
          print('🗑️ Mensagem deletada recebida: $message');
          // ✅ REMOVER MENSAGEM DO CHAT LIST SE FOR A ÚLTIMA
          _removeMessageFromChatList(message);
          break;
        case 'message_reply':
          _messageController.add(message);
          print('💬 Resposta recebida: $message');
          final shouldIncreaseUnread =
              message['should_increase_unread'] ?? true;
          _updateChatOnMessageReceived(message, shouldIncreaseUnread);
          break;
        case 'chat_list_update':
          print('📋 Chat list update recebido: $message');
          final shouldIncreaseUnread = false; // Não aumenta unread para updates

          // Verificar se é uma edição para tratar adequadamente
          final action = message['action']?.toString();
          if (action == 'edit_message') {
            print(
              '   ✅ É uma edição de mensagem - atualizando conteúdo sem mover chat',
            );
            // ✅ ATUALIZAR CONTEÚDO SEM REORDENAR CHAT LIST!
            _updateChatContentOnly(message);
          } else if (action == 'delete_message') {
            print(
              '   ✅ É uma deleção de mensagem - atualizando conteúdo sem mover chat',
            );
            // ✅ ATUALIZAR CONTEÚDO SEM REORDENAR CHAT LIST!
            _updateChatContentOnly(message);
          } else {
            _updateChatOnMessageReceived(message, shouldIncreaseUnread);
          }
          break;
        case 'presence':
          final userId = message['user_id']?.toString();
          final status = message['status']?.toString();
          final ts = message['timestamp'];

          // ✅ Proteção contra eventos fora de ordem (stale)
          int? incomingTs;
          if (ts is int) {
            incomingTs = ts;
          } else if (ts is String) {
            incomingTs = int.tryParse(ts);
          }

          final lastTs = userId != null ? _presenceTimestamps[userId] : null;

          final isStale =
              (incomingTs != null && lastTs != null && incomingTs <= lastTs);

          if (userId != null && status != null) {
            if (isStale) {
              print(
                '⏳ Ignorando presença desatualizada: $userId ts=$incomingTs (last=$lastTs)',
              );
              break;
            }

            if (incomingTs != null) {
              _presenceTimestamps[userId] = incomingTs;
            }

            _userPresenceStatus[userId] = status;
            _presenceController.add({
              'user_id': userId,
              'status': status,
              'timestamp': incomingTs ?? message['timestamp'],
            });
            print('📡 Presença atualizada: $userId -> $status');
          }
          break;
        default:
          print('❓ Unknown message type: ${message['type']}');
      }
    } catch (e) {
      print('❌ Error parsing message: $e - Raw data: $data');
    }
  }

  // ATUALIZAR CHAT COM CONTROLE DE UNREAD - COM DEBUG DETALHADO
  // ✅ ADICIONAR ESTA FUNÇÃO NO ChatService
  static void _updateChatOnMessageReceived(
    Map<String, dynamic> message,
    bool shouldIncreaseUnread,
  ) async {
    try {
      // Tentar pegar IDs primários
      String? fromUserId = message['from']?.toString();
      String? toUserId = message['to']?.toString();

      // Fallback: alguns payloads podem usar sender_id / receiver_id (histórico/offline)
      fromUserId ??= message['sender_id']?.toString();
      toUserId ??= message['receiver_id']?.toString();

      final content = message['content']?.toString() ?? '';
      var currentUserId = await _secureStorage.read(key: 'user_id');

      // Fallback: tenta recarregar auth data se vier nulo
      if (currentUserId == null) {
        final auth = await _loadAuthData();
        currentUserId = auth['userId'];
      }

      // ✅ DETECTAR SE É UM REPLY
      final isReply = message['reply_to_id'] != null;
      if (isReply) {
        print('🔍 ATUALIZANDO CHAT PARA REPLY');
        print('   reply_to_id: ${message['reply_to_id']}');
      }

      // ✅ LÓGICA DE UNREAD CORRIGIDA
      if (fromUserId == currentUserId) {
        // Mensagem enviada por mim - NUNCA aumentar unread
        shouldIncreaseUnread = false;
        print('   🚫 Sou o remetente - unread=false');
      }

      if (currentUserId == null || fromUserId == null || toUserId == null) {
        print(
          '❌ Dados insuficientes para atualizar chat '
          '(from=$fromUserId to=$toUserId me=$currentUserId type=${message['type']})',
        );
        return;
      }

      // ✅ IDENTIFICAR CONTATO
      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      // ✅ SE O CHAT ESTÁ ABERTO, NÃO AUMENTAR UNREAD
      if (_activeChatContactId == contactId) {
        shouldIncreaseUnread = false;
        print('   👀 Chat ativo - unread=false');
      }

      // ✅ BUSCAR INFORMAÇÕES DO CONTATO
      final contactInfo = await _getContactInfo(contactId);

      // ✅ ATUALIZAR CHAT
      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;
        final newUnreadCount = shouldIncreaseUnread
            ? existing.unreadCount + 1
            : existing.unreadCount;

        _chatContacts[contactId] = existing.copyWith(
          name: contactInfo['name'],
          phoneNumber: contactInfo['phone'],
          photo: contactInfo['photo'],
          lastMessageTime: DateTime.now(),
          lastMessage: content,
          unreadCount: newUnreadCount,
          lastMessageIsReply: isReply, // ✅ MARCAR COMO REPLY
        );
      } else {
        _chatContacts[contactId] = ChatContact(
          contactId: contactId,
          name: contactInfo['name'],
          phoneNumber: contactInfo['phone'],
          photo: contactInfo['photo'],
          lastMessageTime: DateTime.now(),
          lastMessage: content,
          unreadCount: shouldIncreaseUnread ? 1 : 0,
          lastMessageIsReply: isReply, // ✅ MARCAR COMO REPLY
        );
      }

      _saveChatsToStorage();
      _chatListController.add(_getSortedChatList());

      print(
        '✅ Chat atualizado: ${contactInfo['name']} (unread: ${_chatContacts[contactId]!.unreadCount})',
      );
    } catch (e) {
      print('❌ Erro ao atualizar chat: $e');
    }
  }

  // ATUALIZAR CHAT PARA EDIÇÃO DE MENSAGEM
  static void _updateChatOnMessageEdit(Map<String, dynamic> message) async {
    try {
      // Tentar pegar IDs primários
      String? fromUserId = message['from']?.toString();
      String? toUserId = message['to']?.toString();

      // Fallback: alguns payloads podem usar sender_id / receiver_id
      fromUserId ??= message['sender_id']?.toString();
      toUserId ??= message['receiver_id']?.toString();

      final content = message['content']?.toString() ?? '';
      var currentUserId = await _secureStorage.read(key: 'user_id');

      // Fallback: tenta recarregar auth data se vier nulo
      if (currentUserId == null) {
        final auth = await _loadAuthData();
        currentUserId = auth['userId'];
      }

      if (currentUserId == null || fromUserId == null || toUserId == null) {
        print('❌ Dados insuficientes para atualizar chat de edição');
        return;
      }

      // ✅ IDENTIFICAR CONTATO (sempre o outro usuário)
      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      // ✅ BUSCAR INFORMAÇÕES DO CONTATO
      final contactInfo = await _getContactInfo(contactId);

      // ✅ ATUALIZAR CHAT EXISTENTE SEM MUDAR UNREAD
      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;
        print('🔍 DEBUG EDIÇÃO CHAT:');
        print('   - Chat: ${contactInfo['name']}');
        print('   - Timestamp ANTES: ${existing.lastMessageTime}');
        print(
          '   - Timestamp DEPOIS: ${existing.lastMessageTime}',
        ); // ✅ VALOR REAL!

        _chatContacts[contactId] = existing.copyWith(
          name: contactInfo['name'],
          phoneNumber: contactInfo['phone'],
          photo: contactInfo['photo'],
          // ✅ NÃO ATUALIZAR lastMessageTime - EDIÇÃO NÃO MOVE CHAT!
          lastMessageTime: existing.lastMessageTime,
          lastMessage: content, // ✅ ATUALIZAR CONTEÚDO DA MENSAGEM EDITADA
          unreadCount: existing.unreadCount, // ✅ MANTER UNREAD ATUAL
          lastMessageIsReply: false, // ✅ NÃO É REPLY
        );
        print(
          '   ✅ Chat atualizado com mensagem editada: ${contactInfo['name']}',
        );
      }

      _saveChatsToStorage();
      // ✅ NÃO REORDENAR EM EDIÇÕES - APENAS ATUALIZAR CONTEÚDO
      _chatListController.add(_chatContacts.values.toList());

      print(
        '✅ Chat de edição atualizado: ${contactInfo['name']} (content: $content)',
      );
    } catch (e) {
      print('❌ Erro ao atualizar chat de edição: $e');
    }
  }

  // ✅ MÉTODO FINAL PARA BUSCAR INFORMAÇÕES DO CONTATO - CORRIGIDO
  static Future<Map<String, dynamic>> _getContactInfo(String contactId) async {
    print('🔍🆕 BUSCA REAL DE NOME PARA: $contactId');

    // ✅ 1. BUSCA NO BACKEND - ENDPOINT CORRIGIDO QUE AGORA FUNCIONA
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');
      if (accessToken == null) {
        print('❌ TOKEN NÃO ENCONTRADO');
        throw Exception('Token não disponível');
      }

      // ✅ ENDPOINT CORRETO: /api/users/:user_id (AGORA FUNCIONA)
      final url = Uri.parse('http://10.0.2.2:4000/api/users/$contactId');
      //final url = Uri.parse('http://192.168.100.17:4000/api/users/$contactId');
      final headers = {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer $accessToken',
      };

      print('🌐📞 CHAMANDO BACKEND: GET $url');

      final response = await http
          .get(url, headers: headers)
          .timeout(Duration(seconds: 10));

      print('📡 RESPOSTA BRUTA:');
      print('   Status: ${response.statusCode}');
      print('   Body: ${response.body}');

      if (response.statusCode == 200) {
        final userData = json.decode(response.body);
        print('📊 DADOS DECODIFICADOS: $userData');

        final userName = userData['name']?.toString();
        final userPhone = userData['phone']?.toString();

        if (userName != null && userName.isNotEmpty && userName != 'null') {
          print('✅✅✅ NOME ENCONTRADO NA BD: "$userName"');
          return {
            'name': userName,
            'phone': userPhone ?? contactId,
            'photo': null,
          };
        } else {
          print('⚠️ Nome vazio ou nulo no backend');
          throw Exception('Nome vazio da BD');
        }
      } else if (response.statusCode == 404) {
        print('❌ USUÁRIO NÃO ENCONTRADO NA BD');
        throw Exception('Usuário não existe na BD');
      } else {
        print('❌ ERRO HTTP: ${response.statusCode}');
        throw Exception('Erro HTTP ${response.statusCode}');
      }
    } catch (e) {
      print('💥 ERRO NA BUSCA DO BACKEND: $e');

      // ✅ 2. FALLBACK: BUSCAR DOS CONTATOS LOCAIS
      try {
        final hasContactPermission = await Permission.contacts.isGranted;
        if (hasContactPermission) {
          print('📱 TENTANDO CONTATOS LOCAIS...');
          final contacts = await FlutterContacts.getContacts(
            withProperties: true,
            withPhoto: true,
          );

          final cleanContactId = contactId.replaceAll(RegExp(r'[^0-9+]'), '');
          print('🔍 Procurando por: $cleanContactId');

          for (final contact in contacts) {
            for (final phone in contact.phones) {
              final cleanPhone = phone.number.replaceAll(
                RegExp(r'[^0-9+]'),
                '',
              );

              // ✅ BUSCA MAIS FLEXÍVEL
              if (cleanPhone.contains(cleanContactId) ||
                  cleanContactId.contains(cleanPhone)) {
                final contactName = contact.displayName.isEmpty
                    ? 'Sem Nome'
                    : contact.displayName;

                print('✅ NOME ENCONTRADO NOS CONTATOS: "$contactName"');
                return {
                  'name': contactName,
                  'phone': phone.number,
                  'photo': contact.photo,
                };
              }
            }
          }
          print('❌ Contato não encontrado nos contatos locais');
        } else {
          print('❌ Sem permissão para contatos');
        }
      } catch (e2) {
        print('💥 ERRO NOS CONTATOS LOCAIS: $e2');
      }

      // ✅ 3. FALLBACK FINAL - NÃO USA O ID, USA NOME TEMPORÁRIO
      print('🆘 USANDO NOME TEMPORÁRIO');
      return {
        'name': 'A Carregar...', // ✅ NÃO USA O ID COMO NOME!
        'phone': contactId,
        'photo': null,
      };
    }
  }

  static void _updateOrCreateChatContact({
    required String contactId,
    required String contactName,
    required String lastMessage,
    required bool shouldIncreaseUnread,
    required String? phoneNumber,
    required Uint8List? photo,
  }) {
    final now = DateTime.now();

    print('   🔧 DEBUG _updateOrCreateChatContact:');
    print('      Contact: $contactName ($contactId)');
    print('      Last Message: $lastMessage');
    print('      Should Increase Unread: $shouldIncreaseUnread');
    print('      Chat já existe?: ${_chatContacts.containsKey(contactId)}');

    if (_chatContacts.containsKey(contactId)) {
      final existing = _chatContacts[contactId]!;
      final newUnreadCount = shouldIncreaseUnread
          ? existing.unreadCount + 1
          : existing.unreadCount;

      print('      Unread Count: ${existing.unreadCount} -> $newUnreadCount');
      print('      🕵️ CHAT JÁ EXISTIA! Quem criou?');

      _chatContacts[contactId] = existing.copyWith(
        name: contactName,
        phoneNumber: phoneNumber,
        photo: photo,
        lastMessageTime: now,
        lastMessage: lastMessage,
        unreadCount: newUnreadCount,
      );
      print('   ✅ Chat existente atualizado (Unread: $newUnreadCount)');
    } else {
      // ✅ DEBUG DETALHADO PARA NOVOS CHATS
      print('      🆕 NOVO CHAT CRIADO!');
      print('      Unread inicial: ${shouldIncreaseUnread ? 1 : 0}');

      _chatContacts[contactId] = ChatContact(
        contactId: contactId,
        name: contactName,
        phoneNumber: phoneNumber,
        photo: photo,
        lastMessageTime: now,
        lastMessage: lastMessage,
        unreadCount: shouldIncreaseUnread ? 1 : 0,
      );
      print('   ✅ Novo chat criado (Unread: ${shouldIncreaseUnread ? 1 : 0})');
    }

    _saveChatsToStorage();
    _chatListController.add(_getSortedChatList());
  }

  static Future<void> rebuildChatsFromHistory() async {
    print('🎯🎯🎯 REBUILD CHATS FROM HISTORY CHAMADO 🎯🎯🎯');

    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');
      if (currentUserId == null) return;

      await _loadChatsFromStorage();

      // ✅ DEBUG: Mostrar unread counts atuais
      _chatContacts.forEach((contactId, chat) {
        print('   📊 Chat: ${chat.name} - Unread: ${chat.unreadCount}');
      });

      print('✅ Rebuild completo - unread counts PRESERVADOS');
      _chatListController.add(_getSortedChatList());
    } catch (e) {
      print('❌ Erro no rebuild: $e');
    }
  }

  // ✅ ATUALIZAR APENAS INFORMAÇÕES DO CONTATO
  static Future<void> _updateContactInfoWithoutResettingUnread(
    String contactId,
  ) async {
    try {
      // ✅ BUSCAR INFORMAÇÕES ATUALIZADAS DO CONTATO
      final contactInfo = await _getContactInfo(contactId);

      if (_chatContacts.containsKey(contactId)) {
        final existingChat = _chatContacts[contactId]!;
        // ✅ MANTÉM O UNREAD COUNT EXISTENTE, APENAS ATUALIZA NOME E FOTO
        _chatContacts[contactId] = existingChat.copyWith(
          name: contactInfo['name'],
          phoneNumber: contactInfo['phone'],
          photo: contactInfo['photo'],
        );
        print(
          '✅ Informações atualizadas para: ${contactInfo['name']} (Unread mantido: ${existingChat.unreadCount})',
        );
      }

      _saveChatsToStorage();
    } catch (e) {
      print('❌ Erro ao atualizar informações do contato $contactId: $e');
    }
  }

  // ✅ Verificar se é possível enviar mensagem (conexão + internet)
  static Future<bool> canSendMessage() async {
    if (_channel == null) {
      print('❌ Não conectado ao WebSocket');
      return false;
    }

    try {
      final result = await InternetAddress.lookup(
        'google.com',
      ).timeout(const Duration(seconds: 3));
      final hasInternet = result.isNotEmpty && result[0].rawAddress.isNotEmpty;
      if (!hasInternet) {
        print('❌ Sem conexão com internet');
      }
      return hasInternet;
    } on SocketException catch (_) {
      print('❌ Sem conexão com internet (SocketException)');
      return false;
    } on TimeoutException catch (_) {
      print('❌ Sem conexão com internet (Timeout)');
      return false;
    } catch (e) {
      print('❌ Erro ao verificar conexão com internet: $e');
      return false;
    }
  }

  static Future<void> sendMessage(
    String toUserId,
    String content, {
    String? tempId,
  }) async {
    final okToSend = await canSendMessage();
    if (!okToSend) {
      throw Exception('Sem conexão com internet para enviar mensagem.');
    }

    if (_channel == null) {
      throw Exception('WebSocket não está conectado.');
    }

    final messageId = tempId ?? _generateMessageId();
    final message = {
      'type': 'message',
      'to': toUserId,
      'content': content,
      'message_id': messageId,
    };

    _sentMessageIds.add(messageId);

    try {
      _channel!.sink.add(json.encode(message));
      print('📤 Sent message to $toUserId (ID: $messageId): $content');

      // ✅ ATUALIZA O CHAT LOCALMENTE (SEM UNREAD)
      _updateChatOnMessageSent(toUserId, content);
    } catch (e) {
      print('❌ Error sending message: $e');
      _sentMessageIds.remove(messageId);
      rethrow;
    }
  }

  // ✅ ATUALIZAR CHAT AO ENVIAR MENSAGEM (SEM UNREAD)
  static void _updateChatOnMessageSent(String toUserId, String content) async {
    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');
      if (currentUserId == null) return;

      final contactInfo = await _getContactInfo(toUserId);

      _updateOrCreateChatContact(
        contactId: toUserId,
        contactName: contactInfo['name'],
        lastMessage: content,
        shouldIncreaseUnread: false, // ✅ MENSAGEM ENVIADA NÃO AUMENTA UNREAD
        phoneNumber: contactInfo['phone'],
        photo: contactInfo['photo'],
      );
    } catch (e) {
      print('❌ Erro ao atualizar chat após enviar mensagem: $e');
    }
  }

  // ✅ MÉTODO PÚBLICO PARA ATUALIZAR CHAT (USADO POR RESPOSTAS)
  static void updateChatAfterReply(String toUserId, String content) async {
    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');
      if (currentUserId == null) return;

      final contactInfo = await _getContactInfo(toUserId);

      _updateOrCreateChatContact(
        contactId: toUserId,
        contactName: contactInfo['name'],
        lastMessage: content,
        shouldIncreaseUnread: false, // ✅ REPLY ENVIADO NÃO AUMENTA UNREAD
        phoneNumber: contactInfo['phone'],
        photo: contactInfo['photo'],
      );

      print('✅ Chat atualizado após reply para: ${contactInfo['name']}');
    } catch (e) {
      print('❌ Erro ao atualizar chat após reply: $e');
    }
  }

  static void sendTypingIndicator(String toUserId, bool isTyping) {
    if (_channel == null) return;

    final message = {
      'type': 'typing',
      'to': toUserId,
      'is_typing': isTyping,
      'message_id': 'typing_${DateTime.now().millisecondsSinceEpoch}',
    };

    _channel!.sink.add(json.encode(message));
    print('⌨️ Sent typing indicator to $toUserId: $isTyping');
  }

  static Future<List<Map<String, dynamic>>> loadChatHistory(
    String contactUserId,
  ) async {
    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (currentUserId == null) {
        print('❌ User ID não encontrado no SecureStorage');
        return await _loadChatHistoryFromStorage('unknown', contactUserId);
      }

      final url = Uri.parse(
        'http://10.0.2.2:4000/api/messages/history/$currentUserId/$contactUserId',
      );

      //final url = Uri.parse(
      //  'http://192.168.100.17:4000/api/messages/history/$currentUserId/$contactUserId',
      //);

      print('📨 Carregando histórico: $currentUserId -> $contactUserId');

      final accessToken = await _secureStorage.read(key: 'access_token');
      final headers = {
        'Content-Type': 'application/json',
        if (accessToken != null) 'Authorization': 'Bearer $accessToken',
      };

      final response = await http.get(url, headers: headers);
      print('📡 Response status: ${response.statusCode}');

      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final messages = (data['messages'] as List<dynamic>? ?? []);
        print('✅ Histórico carregado: ${messages.length} mensagens');

        // ✅ Salvar cópia local para uso offline
        await _saveChatHistoryToStorage(currentUserId, contactUserId, messages);

        return messages.cast<Map<String, dynamic>>();
      } else {
        print('❌ Erro ao carregar histórico: ${response.statusCode}');
        return await _loadChatHistoryFromStorage(currentUserId, contactUserId);
      }
    } catch (e) {
      print('❌ Erro loadChatHistory: $e');

      // Fallback: tentar histórico local em caso de erro (inclui sem internet)
      try {
        final currentUserId = await _secureStorage.read(key: 'user_id');
        return await _loadChatHistoryFromStorage(
          currentUserId ?? 'unknown',
          contactUserId,
        );
      } catch (e2) {
        print('❌ Erro ao carregar histórico local: $e2');
        return [];
      }
    }
  }

  // ✅ Helpers para histórico offline
  static String _historyStorageKey(String meId, String contactId) {
    return 'chat_history_${meId}_$contactId';
  }

  static Future<void> _saveChatHistoryToStorage(
    String meId,
    String contactId,
    List<dynamic> messages,
  ) async {
    try {
      final key = _historyStorageKey(meId, contactId);
      final jsonData = json.encode(messages);
      await _secureStorage.write(key: key, value: jsonData);
      print('💾 Histórico salvo localmente ($meId <-> $contactId)');
    } catch (e) {
      print('❌ Erro ao salvar histórico local: $e');
    }
  }

  static Future<List<Map<String, dynamic>>> _loadChatHistoryFromStorage(
    String meId,
    String contactId,
  ) async {
    try {
      final key = _historyStorageKey(meId, contactId);
      final raw = await _secureStorage.read(key: key);
      if (raw == null) {
        print('📂 Nenhum histórico local para $meId <-> $contactId');
        return [];
      }
      final data = json.decode(raw) as List<dynamic>;
      print(
        '📂 Histórico local carregado: ${data.length} mensagens ($meId <-> $contactId)',
      );
      return data.cast<Map<String, dynamic>>();
    } catch (e) {
      print('❌ Erro ao carregar histórico local: $e');
      return [];
    }
  }

  static final Map<String, DateTime> _lastMarkAsReadCall = {};
  static final Duration _markAsReadCooldown = Duration(seconds: 2);

  static void markChatAsRead(String contactId) {
    final now = DateTime.now();
    final lastCall = _lastMarkAsReadCall[contactId];

    // ✅ IMPEDIR CHAMADAS MÚLTIPLAS EM CURTO PERÍODO
    if (lastCall != null && now.difference(lastCall) < Duration(seconds: 2)) {
      print('⏳ markChatAsRead ignorado (cooldown) para: $contactId');
      return;
    }

    _lastMarkAsReadCall[contactId] = now;

    print('📖📖📖 MARK CHAT AS READ 📖📖📖');
    print('   ContactId: $contactId');
    print('   Razão: Chat aberto pelo usuário');

    if (_chatContacts.containsKey(contactId)) {
      final currentChat = _chatContacts[contactId]!;

      if (currentChat.unreadCount > 0) {
        print('   🔄 Unread: ${currentChat.unreadCount} -> 0');
        _chatContacts[contactId] = currentChat.copyWith(unreadCount: 0);
        _saveChatsToStorage();
        _chatListController.add(_getSortedChatList());
        print('   ✅ Chat marcado como lido: ${currentChat.name}');
      } else {
        print('   ℹ️  Chat já estava como lido: ${currentChat.name}');
      }
    } else {
      print('   ❌ Chat não encontrado: $contactId');
    }
  }

  // ✅ NOVO: marcar como lido SEM cooldown (para uso dentro do chat aberto)
  static void markChatAsReadImmediate(String contactId) {
    print('📖📖📖 MARK CHAT AS READ IMMEDIATE 📖📖📖');
    print('   ContactId: $contactId');

    _lastMarkAsReadCall.remove(contactId); // ignora cooldown

    if (_chatContacts.containsKey(contactId)) {
      final currentChat = _chatContacts[contactId]!;

      if (currentChat.unreadCount > 0) {
        print('   🔄 Unread (immediate): ${currentChat.unreadCount} -> 0');
        _chatContacts[contactId] = currentChat.copyWith(unreadCount: 0);
        _saveChatsToStorage();
        _chatListController.add(_getSortedChatList());
        print('   ✅ Chat marcado como lido (immediate): ${currentChat.name}');
      } else {
        print(
          '   ℹ️  Chat já estava como lido (immediate): ${currentChat.name}',
        );
      }
    } else {
      print('   ❌ Chat não encontrado (immediate): $contactId');
    }
  }

  static Future<void> markMessagesRead(String contactId) async {
    try {
      final meId = await _secureStorage.read(key: 'user_id');
      final token = await _secureStorage.read(key: 'access_token');
      if (meId == null || token == null) return;
      final url = Uri.parse(
        'http://10.0.2.2:4000/api/messages/mark_read/$meId/$contactId',
      );
      //final url = Uri.parse(
      //  'http://192.168.100.17:4000/api/messages/mark_read/$meId/$contactId',
      //);

      final headers = {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer $token',
      };
      final res = await http.post(url, headers: headers);
      print('📡 markMessagesRead response: ${res.statusCode}');
    } catch (e) {
      print('❌ markMessagesRead error: $e');
    }
  }

  static void _cleanOldMarkAsReadCalls() {
    final now = DateTime.now();
    final toRemove = <String>[];

    _lastMarkAsReadCall.forEach((contactId, timestamp) {
      if (now.difference(timestamp) > Duration(minutes: 5)) {
        toRemove.add(contactId);
      }
    });

    toRemove.forEach(_lastMarkAsReadCall.remove);
  }

  static List<ChatContact> _getSortedChatList() {
    print('🔍 DEBUG: _getSortedChatList() chamado - reordenando chats...');
    return _chatContacts.values.toList()
      ..sort((a, b) => b.lastMessageTime.compareTo(a.lastMessageTime));
  }

  // ✅ STORAGE METHODS
  static Future<void> _saveChatsToStorage() async {
    try {
      final chatsMap = {};
      _chatContacts.forEach((key, value) {
        chatsMap[key] = value.toMap();
      });
      final jsonData = json.encode(chatsMap);
      await _secureStorage.write(key: 'chat_contacts', value: jsonData);
      print('💾 Chats salvos no storage: ${_chatContacts.length} chats');
    } catch (e) {
      print('❌ Erro ao salvar chats: $e');
    }
  }

  static Future<void> _loadChatsFromStorage() async {
    try {
      final stored = await _secureStorage.read(key: 'chat_contacts');
      if (stored != null) {
        final Map<String, dynamic> chatsMap = json.decode(stored);
        _chatContacts.clear();

        chatsMap.forEach((key, value) {
          try {
            _chatContacts[key] = ChatContact.fromMap(value);
            print(
              '📂 Carregado chat: ${_chatContacts[key]!.name} (Unread: ${_chatContacts[key]!.unreadCount})',
            );

            // ✅ DEBUG EXTRA - Mostra TODOS os dados do chat
            print('   🔍 Dados completos: ${_chatContacts[key]!.toMap()}');
          } catch (e) {
            print('❌ Erro ao carregar chat $key: $e');
          }
        });

        _chatListController.add(_getSortedChatList());
        print('📂 Chats carregados do storage: ${_chatContacts.length}');
      } else {
        print('📂 Nenhum chat encontrado no storage');
      }
    } catch (e) {
      print('❌ Erro ao carregar chats: $e');
    }
  }

  static void disconnect() {
    _isManualDisconnect = true;
    _isReconnecting = false;
    _reconnectAttempts = 0;
    _sentMessageIds.clear();
    _stopHeartbeat();
    _channel?.sink.close();
    _channel = null;
    print('🔌 WebSocket disconnected manually');
  }

  // ✅ SISTEMA DE HEARTBEAT - OTIMIZADO PARA BACKGROUND
  static void _startHeartbeat() {
    _stopHeartbeat(); // Garantir que não há múltiplos timers

    // Enviar heartbeat a cada 20 segundos (mais frequente para garantir em background)
    _heartbeatTimer = Timer.periodic(Duration(seconds: 20), (timer) {
      if (_channel != null) {
        try {
          final heartbeatMsg = json.encode({'type': 'heartbeat'});
          _channel!.sink.add(heartbeatMsg);
          print('💓 Heartbeat enviado (background/foreground)');
        } catch (e) {
          print('❌ Erro ao enviar heartbeat: $e');
          // Se falhar, tentar reconectar
          if (!_isManualDisconnect) {
            print('🔄 Tentando reconectar após falha de heartbeat...');
            connect();
          }
        }
      } else {
        print('💓 WebSocket null, parando heartbeat');
        _stopHeartbeat();
      }
    });
  }

  // ✅ ENVIAR HEARTBEAT MANUALMENTE (para background manager)
  static Future<bool> sendHeartbeat() async {
    if (_channel == null) return false;

    try {
      final heartbeatMsg = json.encode({'type': 'heartbeat'});
      _channel!.sink.add(heartbeatMsg);
      print('💓 Heartbeat enviado manualmente');
      return true;
    } catch (e) {
      print('❌ Erro ao enviar heartbeat manual: $e');
      return false;
    }
  }

  // ✅ VERIFICAR SE ESTÁ CONECTADO
  static bool isWebSocketConnected() {
    return _channel != null;
  }

  // ✅ ENVIAR PRESENÇA MANUALMENTE (Online/Offline)
  static void sendPresence(String status) {
    if (_channel == null) return;

    try {
      final msg = json.encode({
        'type': 'presence_update',
        'status': status,
        'timestamp': DateTime.now().millisecondsSinceEpoch ~/ 1000,
      });
      _channel!.sink.add(msg);
      print('📡 Presença manual enviada: $status');
    } catch (e) {
      print('❌ Erro ao enviar presença manual: $e');
    }
  }

  static void _stopHeartbeat() {
    _heartbeatTimer?.cancel();
    _heartbeatTimer = null;
  }

  // ✅ Obter status de presença de um usuário
  static Future<Map<String, dynamic>?> getUserPresence(String userId) async {
    try {
      print('🔍 getUserPresence chamado para: $userId');

      // Primeiro verificar cache local
      if (_userPresenceStatus.containsKey(userId)) {
        final status = _userPresenceStatus[userId];
        print('📦 Status em cache: $status');
        if (status == 'online') {
          return {'status': 'online', 'last_seen': null};
        }
      }

      // Buscar do servidor
      final accessToken = await _secureStorage.read(key: 'access_token');
      if (accessToken == null) {
        print('❌ Token não encontrado');
        return {'status': 'offline', 'last_seen': null};
      }

      final url = Uri.parse('http://10.0.2.2:4000/api/presence/$userId');
      //final url = Uri.parse('http://192.168.100.17:4000/api/presence/$userId');

      final headers = {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer $accessToken',
      };

      print('🌐 Chamando API: $url');
      final response = await http
          .get(url, headers: headers)
          .timeout(Duration(seconds: 5));

      print('📡 Response status: ${response.statusCode}');
      print('📡 Response body: ${response.body}');

      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final status = data['status']?.toString();
        final lastSeen = data['last_seen'];

        print('✅ Status recebido: $status, last_seen: $lastSeen');

        // Atualizar cache
        if (status != null) {
          _userPresenceStatus[userId] = status;
        }

        return {'status': status ?? 'offline', 'last_seen': lastSeen};
      } else {
        print('❌ Erro HTTP: ${response.statusCode}');
        return {'status': 'offline', 'last_seen': null};
      }
    } catch (e, stackTrace) {
      print('❌ Erro ao obter presença: $e');
      print('📚 Stack trace: $stackTrace');
      return {'status': 'offline', 'last_seen': null};
    }
  }

  // ✅ GETTERS
  static Stream<List<ChatContact>> get chatListStream =>
      _chatListController.stream;
  static List<ChatContact> get currentChatList => _getSortedChatList();
  static Stream<Map<String, dynamic>> get messageStream =>
      _messageController.stream;
  static Stream<Map<String, dynamic>> get typingStream =>
      _typingController.stream;
  static Stream<Map<String, dynamic>> get presenceStream =>
      _presenceController.stream;
  static bool get isConnected => _channel != null;

  static Future<Map<String, String?>> _loadAuthData() async {
    final token = await _secureStorage.read(key: 'access_token');
    var userId = await _secureStorage.read(key: 'user_id');

    if (userId == null && token != null) {
      final extracted = _tryExtractUserIdFromJwt(token);
      if (extracted != null) {
        await _secureStorage.write(key: 'user_id', value: extracted);
        userId = extracted;
      }
    }

    if (token == null || userId == null) {
      final prefs = await SharedPreferences.getInstance();
      final legacyToken = prefs.getString('access_token');
      final legacyUserId = prefs.getString('user_id');

      if (legacyToken != null && legacyUserId != null) {
        await _secureStorage.write(key: 'access_token', value: legacyToken);
        await _secureStorage.write(key: 'user_id', value: legacyUserId);
        return {'token': legacyToken, 'userId': legacyUserId};
      }
    }

    return {'token': token, 'userId': userId};
  }

  static String? _tryExtractUserIdFromJwt(String token) {
    try {
      final parts = token.split('.');
      if (parts.length < 2) return null;
      final payloadB64 = parts[1];
      final normalized = _normalizeBase64Url(payloadB64);
      final payloadBytes = base64Url.decode(normalized);
      final payload = json.decode(utf8.decode(payloadBytes));
      final uid = payload['user_id'];
      if (uid == null) return null;
      return uid.toString();
    } catch (_) {
      return null;
    }
  }

  static String _normalizeBase64Url(String input) {
    final rem = input.length % 4;
    if (rem == 2) return '$input==';
    if (rem == 3) return '$input=';
    if (rem == 1) return '$input===';
    return input;
  }

  // ✅ ATUALIZAR APENAS CONTEÚDO DO CHAT SEM REORDENAR
  static void _updateChatContentOnly(Map<String, dynamic> message) async {
    try {
      String? fromUserId = message['from']?.toString();
      String? toUserId = message['to']?.toString();
      final content = message['content']?.toString() ?? '';
      var currentUserId = await _secureStorage.read(key: 'user_id');

      if (currentUserId == null || fromUserId == null || toUserId == null) {
        print('❌ Dados insuficientes para atualizar conteúdo do chat');
        return;
      }

      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;
        print('🔧 ATUALIZANDO CONTEÚDO DO CHAT (sem reordenar):');
        print('   - Chat: ${existing.name}');
        print('   - Novo conteúdo: $content');

        _chatContacts[contactId] = existing.copyWith(
          name: existing.name,
          phoneNumber: existing.phoneNumber,
          photo: existing.photo,
          lastMessageTime: existing.lastMessageTime, // ✅ PRESERVAR TIMESTAMP!
          lastMessage: content, // ✅ ATUALIZAR APENAS CONTEÚDO
          unreadCount: existing.unreadCount, // ✅ MANTER UNREAD
          lastMessageIsReply: false,
        );
        print('   ✅ Conteúdo do chat atualizado sem mover posição');
      }

      // ✅ ATUALIZAR FRONTEND SEM REORDENAR
      _chatListController.add(_chatContacts.values.toList());
      _saveChatsToStorage();
    } catch (e) {
      print('❌ Erro ao atualizar conteúdo do chat: $e');
    }
  }

  // ✅ REMOVER MENSAGEM DO CHAT LIST QUANDO DELETADA
  static void _removeMessageFromChatList(Map<String, dynamic> message) async {
    try {
      String? fromUserId = message['sender_id']?.toString();
      String? toUserId = message['receiver_id']?.toString();
      var currentUserId = await _secureStorage.read(key: 'user_id');

      if (currentUserId == null || fromUserId == null || toUserId == null) {
        print('❌ Dados insuficientes para remover mensagem do chat');
        return;
      }

      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;
        print('🗑️ REMOVENDO MENSAGEM DO CHAT LIST:');
        print('   - Chat: ${existing.name}');
        print('   - Mensagem ID: ${message['message_id']}');

        // ✅ SE A MENSAGEM DELETADA FOR A ÚLTIMA, ATUALIZAR PARA ANTERIOR
        // SE NÃO, MANTER ÚLTIMA ATUAL
        _updateChatListAfterDeletion(contactId);
        print('   ✅ Chat list atualizado após deleção');
      }

      // ✅ ATUALIZAR FRONTEND
      _chatListController.add(_chatContacts.values.toList());
      _saveChatsToStorage();
    } catch (e) {
      print('❌ Erro ao remover mensagem do chat: $e');
    }
  }

  // ✅ ATUALIZAR CHAT LIST APÓS DELEÇÃO (BUSCAR ÚLTIMA MENSAGEM RESTANTE)
  static void _updateChatListAfterDeletion(String contactId) async {
    try {
      // Buscar última mensagem não deletada deste contato
      final response = await http.get(
        Uri.parse(
          'http://10.0.2.2:4000/api/messages/history/$contactId?limit=1',
        ),
        headers: {
          'Authorization':
              'Bearer ${await _secureStorage.read(key: 'access_token')}',
          'Content-Type': 'application/json',
        },
      );

      if (response.statusCode == 200) {
        final data = json.decode(response.body);
        final messages = data['messages'] as List<dynamic>;

        if (_chatContacts.containsKey(contactId)) {
          final existing = _chatContacts[contactId]!;

          if (messages.isNotEmpty) {
            // ✅ TEM MENSAGENS RESTANTES - USAR A ÚLTIMA
            final lastMessage = messages.first;
            _chatContacts[contactId] = existing.copyWith(
              name: existing.name,
              phoneNumber: existing.phoneNumber,
              photo: existing.photo,
              lastMessageTime: DateTime.parse(
                lastMessage['sent_at'],
              ), // ✅ USAR TIMESTAMP DA ÚLTIMA
              lastMessage: lastMessage['content'],
              unreadCount: existing.unreadCount,
              lastMessageIsReply: lastMessage['reply_to_id'] != null,
            );
            print(
              '   ✅ Última mensagem restante usada: ${lastMessage['content']}',
            );
          } else {
            // ✅ NÃO TEM MENSAGENS - MOSTRAR MENSAGEM DE APAGADA
            _chatContacts[contactId] = existing.copyWith(
              name: existing.name,
              phoneNumber: existing.phoneNumber,
              photo: existing.photo,
              lastMessageTime:
                  existing.lastMessageTime, // ✅ MANTER TIMESTAMP ANTIGO
              lastMessage: 'Esta mensagem foi apagada',
              unreadCount: existing.unreadCount,
              lastMessageIsReply: false,
            );
            print('   ✅ Chat limpo - mostrando "Esta mensagem foi apagada"');
          }
        }
      }
    } catch (e) {
      print('❌ Erro ao buscar últimas mensagens após deleção: $e');
    }
  }
}
