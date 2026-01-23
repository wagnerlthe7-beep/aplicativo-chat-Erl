// lib/chat_service.dart - VERSÃO FINAL CORRIGIDA
import 'dart:convert';
import 'dart:async';
import 'dart:typed_data';
import 'dart:io';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:web_socket_channel/io.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:shared_preferences/shared_preferences.dart';
import 'package:http/http.dart' as http;
import 'package:uuid/uuid.dart';
import 'chat_model.dart';
import 'contacts_helper.dart';
import 'models/pending_message.dart';
import 'services/pending_messages_storage.dart';

class ChatService {
  static WebSocketChannel? _channel;
  static String? _currentUserId;
  static const _secureStorage = FlutterSecureStorage();

  static final Uuid _uuid = Uuid();
  static bool _isReconnecting = false;
  static bool _isConnecting =
      false; // ✅ Lock para evitar múltiplas conexões simultâneas
  static bool _isManualDisconnect = false;
  static int _reconnectAttempts = 0;
  static const int _maxReconnectAttempts = 5;

  // ✅ Flag para evitar tentativas redundantes quando já sabemos que caiu
  static bool isServerDown = false;

  static final _messageController =
      StreamController<Map<String, dynamic>>.broadcast();
  static final _typingController =
      StreamController<Map<String, dynamic>>.broadcast();
  static final _presenceController =
      StreamController<Map<String, dynamic>>.broadcast();

  // ✅ Stream de Status de Conexão (Novo)
  static final _connectionStatusController = StreamController<bool>.broadcast();
  static Stream<bool> get connectionStatusStream =>
      _connectionStatusController.stream;

  static final Map<String, int> _presenceTimestamps = {};
  static final Set<String> _sentMessageIds = {};

  // ✅ Controle de presença
  static Timer? _heartbeatTimer;
  static final Map<String, String> _userPresenceStatus =
      {}; // user_id -> status

  static final Map<String, String> _contactIdToPhoneCache = {};

  // ✅ SISTEMA DE CHATS DINÂMICO
  static final _chatListController =
      StreamController<List<ChatContact>>.broadcast();
  static final Map<String, ChatContact> _chatContacts = {};
  static String? _activeChatContactId;

  // ✅ Timer para debounce de salvamento
  static Timer? _saveDebounceTimer;

  static String _generateMessageId() {
    return 'msg_${DateTime.now().millisecondsSinceEpoch}_${_uuid.v4().substring(0, 8)}';
  }

  static Future<bool> connect() async {
    // ✅ EVITAR MÚLTIPLAS CONEXÕES SIMULTÂNEAS
    if (_isConnecting) {
      print('⏳ Conexão já em progresso, ignorando chamada duplicada...');
      return false;
    }

    // ✅ VERIFICAR SE JÁ ESTÁ CONECTADO E FUNCIONANDO
    if (_channel != null && !_isReconnecting) {
      try {
        // Tentar enviar um ping para verificar se a conexão está realmente ativa
        _channel!.sink.add(json.encode({'type': 'heartbeat'}));
        print('✅ WebSocket já conectado e funcionando');
        return true;
      } catch (e) {
        // Se falhar, a conexão está morta - limpar e reconectar
        print('⚠️ WebSocket existente está morto, limpando e reconectando...');
        _channel = null;
      }
    }

    _isManualDisconnect = false;
    _isConnecting = true; // ✅ LOCK: Marcar que estamos conectando

    try {
      _reconnectAttempts++;
      final authData = await _loadAuthData();
      final token = authData['token'];
      _currentUserId = authData['userId'];

      if (token == null || _currentUserId == null) {
        print('❌ No authentication data found');
        return false;
      }

      // Usando WebSocket.connect diretamento para ter controle de timeout e erros
      // final url = 'ws://10.0.2.2:4000/ws?token=$token';
      final url = 'ws://192.168.100.35:4000/ws?token=$token';

      // print('🔌 Tentando conectar WebSocket...');

      // ✅ Conexão manual segura com timeout
      final ws = await WebSocket.connect(url).timeout(Duration(seconds: 5));
      _channel = IOWebSocketChannel(ws);

      _channel!.stream.listen(
        _handleIncomingMessage,
        onError: (error) {
          print('❌ WebSocket stream error: $error');
          _handleDisconnect();
        },
        onDone: () {
          print('🔌 WebSocket disconnected (Done)');
          _handleDisconnect();
        },
      );

      _isReconnecting = false;
      // NÃO resetar tentativas aqui, apenas após receber 'welcome' ou conexão estável
      // _reconnectAttempts = 0;
      print('✅ WebSocket connected for user $_currentUserId');

      // ✅ RESETAR TIMESTAMPS ao conectar para evitar problemas de stale events
      _presenceTimestamps.clear();
      print('🔄 Presence timestamps resetados ao conectar');

      // ✅ RESETAR VARIÁVEIS DE RECONEXÃO após conexão bem-sucedida
      _reconnectAttempts = 0;
      _isReconnecting = false;
      _isConnecting = false; // ✅ UNLOCK: Conexão estabelecida
      print('🔄 Variáveis de reconexão resetadas');

      // ✅ INICIAR SISTEMA DE HEARTBEAT
      _startHeartbeat();

      isServerDown = false; // ✅ Conexão estabelecida

      // ✅ Notificar que estamos ONLINE
      _connectionStatusController.add(true);

      return true;
    } on SocketException catch (_) {
      // ✅ Captura erro de servidor indisponível
      isServerDown = true;
      _isConnecting = false; // ✅ UNLOCK em caso de erro
      print(
        '⚠️ Servidor indisponível (SocketException) - Modo Offline Ativado',
      );
      return false;
    } on TimeoutException catch (_) {
      // ✅ Captura timeout
      isServerDown = true;
      _isConnecting = false; // ✅ UNLOCK em caso de erro
      print('⚠️ Timeout na conexão WebSocket - Modo Offline Ativado');
      return false;
    } catch (e) {
      _isConnecting = false; // ✅ UNLOCK em caso de erro
      print('❌ Erro genérico na conexão WebSocket: $e');
      return false;
    }
  }

  static void _handleDisconnect() {
    _channel = null;

    // ✅ Notificar OFFLINE
    _connectionStatusController.add(false);

    if (_isManualDisconnect) {
      print('🔌 Desconexão manual - não reconectando automaticamente');
      return;
    }

    // ✅ Quando desconectar (perda de internet / WS fechado),
    // marcar TODOS os contatos locais como offline para o cliente atual.
    if (_userPresenceStatus.isNotEmpty) {
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
      print('🔍 [WS DEBUG] Mensagem recebida: $message');

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

          // ✅ Notificar reconexão para atualizar presença
          _connectionStatusController.add(true);
          break;
        case 'message':
          _messageController.add(message);
          final shouldIncreaseUnread =
              message['should_increase_unread'] ?? true;
          _updateChatOnMessageReceived(message, shouldIncreaseUnread);

          // ✅ OFFLINE-FIRST: Atualizar status de mensagem pendente quando receber confirmação
          final tempMessageId = message['message_id']?.toString();
          final dbMessageIdStr = message['db_message_id']?.toString();
          if (tempMessageId != null && dbMessageIdStr != null) {
            // ✅ Executar de forma assíncrona sem bloquear
            updateMessageStatusFromServer(
              tempMessageId,
              'sent',
              dbMessageId: dbMessageIdStr,
            ).catchError((e) => print('❌ Erro ao atualizar status: $e'));
          }
          break;
        case 'message_delivered':
          _messageController.add(message);

          // ✅ OFFLINE-FIRST: Atualizar status para 'delivered'
          final tempMessageId = message['message_id']?.toString();
          if (tempMessageId != null) {
            // ✅ Executar de forma assíncrona sem bloquear
            updateMessageStatusFromServer(
              tempMessageId,
              'delivered',
            ).catchError((e) => print('❌ Erro ao atualizar status: $e'));
          }
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
          // ✅ SÓ ATUALIZAR CHAT LIST SE FOR A ÚLTIMA MENSAGEM
          _updateChatContentOnlyWithDeletedMessageIfLast(message);
          break;
        case 'message_reply':
          _messageController.add(message);
          print('💬 Resposta recebida: $message');
          final shouldIncreaseUnread =
              message['should_increase_unread'] ?? true;
          _updateChatOnMessageReceived(message, shouldIncreaseUnread);
          break;
        case 'chat_list_update':
          final shouldIncreaseUnread = false; // Não aumenta unread para updates

          // Verificar se é uma edição para tratar adequadamente
          final action = message['action']?.toString();
          if (action == 'edit_message') {
            // ✅ ATUALIZAR CONTEÚDO SEM REORDENAR CHAT LIST!
            _updateChatContentOnly(message);
          } else if (action == 'delete_message') {
            // ✅ VERIFICAR SE É A ÚLTIMA MENSAGEM ANTES DE ATUALIZAR
            _updateChatContentOnlyWithDeletedMessageIfLast(message);
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
            print(
              '🔍 [PRESENCE SERVICE] Adicionando ao stream: userId=$userId, status=$status',
            );
            _presenceController.add({
              'user_id': userId,
              'status': status,
              'timestamp': incomingTs ?? message['timestamp'],
            });
            print('📡 Presença atualizada: $userId -> $status');
          }
          break;
        case 'typing':
          print('⌨️ Evento typing recebido: $message');
          _typingController.add(message);
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
      if (isReply) {}

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
      }

      _saveChatsToStorage();
      // ✅ NÃO REORDENAR EM EDIÇÕES - APENAS ATUALIZAR CONTEÚDO
      _chatListController.add(_chatContacts.values.toList());
    } catch (e) {
      print('❌ Erro ao atualizar chat de edição: $e');
    }
  }

  // ✅ MÉTODO FINAL PARA BUSCAR INFORMAÇÕES DO CONTATO
  static Future<Map<String, dynamic>> _getContactInfo(
    String contactId, {
    Map<String, String>? localContacts,
  }) async {
    String? phone =
        _contactIdToPhoneCache[contactId] ??
        _chatContacts[contactId]?.phoneNumber;

    // ✅ 1. TENTAR BUSCAR NO BACKEND SE NÃO TIVERMOS O TELEFONE
    if (phone == null || phone == contactId) {
      try {
        final accessToken = await _secureStorage.read(key: 'access_token');
        if (accessToken != null) {
          final url = Uri.parse(
            'http://192.168.100.35:4000/api/users/$contactId',
          );
          final headers = {
            'Content-Type': 'application/json',
            'Authorization': 'Bearer $accessToken',
          };

          final response = await http
              .get(url, headers: headers)
              .timeout(Duration(seconds: 5));

          if (response.statusCode == 200) {
            final userData = json.decode(response.body);
            phone = userData['phone']?.toString();
            if (phone != null && phone.isNotEmpty) {
              _contactIdToPhoneCache[contactId] = phone;
              _saveContactCacheToStorage();
            }
          } else {
            print('   ⚠️ Backend retornou status: ${response.statusCode}');
          }
        } else {
          print('   ⚠️ Access token não disponível');
        }
      } catch (e) {
        print('   ⚠️ Falha ao buscar telefone no backend para $contactId: $e');
      }
    } else {
      print('   ✅ Usando phone do cache: $phone');
    }

    // ✅ 2. SE TEMOS O TELEFONE (CACHED OU BACKEND), BUSCAR NA AGENDA
    if (phone != null && phone.isNotEmpty && phone != contactId) {
      try {
        final cleanPhone = phone.replaceAll(RegExp(r'[\s\-\(\)]'), '');

        // ✅ Usar agenda pré-carregada se disponível, senão carregar
        final contacts =
            localContacts ?? await ContactsHelper.getLocalContactsMap();

        String finalDisplayName;
        if (contacts.containsKey(cleanPhone)) {
          finalDisplayName = contacts[cleanPhone]!;
        } else {
          // Tentar também sem o código do país
          String? alternativePhone;
          if (cleanPhone.startsWith('+')) {
            alternativePhone = cleanPhone.substring(1);
          } else if (cleanPhone.startsWith('258')) {
            alternativePhone = cleanPhone.substring(3);
          }

          if (alternativePhone != null &&
              contacts.containsKey(alternativePhone)) {
            finalDisplayName = contacts[alternativePhone]!;
          } else {
            finalDisplayName = phone;
          }
        }

        return {'name': finalDisplayName, 'phone': phone, 'photo': null};
      } catch (e) {
        print('   ❌ Erro ao buscar na agenda: $e');
        return {'name': phone, 'phone': phone, 'photo': null};
      }
    }

    // Fallback último caso: usar o contactId
    print('   ⚠️ Fallback: usando contactId como nome');
    return {'name': contactId, 'phone': contactId, 'photo': null};
  }

  // ✅ SALVAR/CARREGAR CACHE DE TELEFONES
  static Future<void> _saveContactCacheToStorage() async {
    try {
      final jsonData = json.encode(_contactIdToPhoneCache);
      await _secureStorage.write(key: 'contact_phone_cache', value: jsonData);
    } catch (e) {
      print('❌ Erro ao salvar cache de telefones: $e');
    }
  }

  static Future<void> _loadContactCacheFromStorage() async {
    try {
      final stored = await _secureStorage.read(key: 'contact_phone_cache');
      if (stored != null) {
        final Map<String, dynamic> data = json.decode(stored);
        data.forEach((key, value) {
          _contactIdToPhoneCache[key] = value.toString();
        });
      }
    } catch (e) {
      print('❌ Erro ao carregar cache de telefones: $e');
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

    if (_chatContacts.containsKey(contactId)) {
      final existing = _chatContacts[contactId]!;
      final newUnreadCount = shouldIncreaseUnread
          ? existing.unreadCount + 1
          : existing.unreadCount;

      _chatContacts[contactId] = existing.copyWith(
        name: contactName,
        phoneNumber: phoneNumber,
        photo: photo,
        lastMessageTime: now,
        lastMessage: lastMessage,
        unreadCount: newUnreadCount,
      );
    } else {
      _chatContacts[contactId] = ChatContact(
        contactId: contactId,
        name: contactName,
        phoneNumber: phoneNumber,
        photo: photo,
        lastMessageTime: now,
        lastMessage: lastMessage,
        unreadCount: shouldIncreaseUnread ? 1 : 0,
      );
    }

    _saveChatsToStorage();
    _chatListController.add(_getSortedChatList());
  }

  static Future<void> rebuildChatsFromHistory() async {
    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');
      if (currentUserId == null) return;

      await loadLocalChats();

      // ✅ DEBUG: Mostrar unread counts atuais
      _chatContacts.forEach((contactId, chat) {});
      _chatListController.add(_getSortedChatList());

      // ✅ ⚡ CARREGAR AGENDA UMA VEZ E REUTILIZAR
      print('🔄 Iniciando atualização de dados dos contatos em background...');
      final localContacts = await ContactsHelper.getLocalContactsMap();
      print('📱 ${localContacts.length} contatos locais mapeados.');

      for (final contactId in _chatContacts.keys) {
        _updateContactInfoWithoutResettingUnread(contactId, localContacts);
      }
    } catch (e) {
      print('❌ Erro no rebuild: $e');
    }
  }

  // ✅ ATUALIZAR APENAS INFORMAÇÕES DO CONTATO (com agenda pré-carregada)
  static Future<void> _updateContactInfoWithoutResettingUnread(
    String contactId,
    Map<String, String> localContacts,
  ) async {
    try {
      // ✅ BUSCAR INFORMAÇÕES ATUALIZADAS DO CONTATO (usando agenda pré-carregada)
      final contactInfo = await _getContactInfo(
        contactId,
        localContacts: localContacts,
      );

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

      // ✅ IMPORTANTE: Notificar a UI sobre a mudança!
      _chatListController.add(_getSortedChatList());
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

  // ✅ OFFLINE-FIRST: Sempre salvar localmente primeiro
  static Future<void> sendMessage(
    String toUserId,
    String content, {
    String? tempId,
  }) async {
    final currentUserId = await _secureStorage.read(key: 'user_id');
    if (currentUserId == null) {
      throw Exception('User ID não encontrado');
    }

    final messageId = tempId ?? _generateMessageId();

    // ✅ 1. SEMPRE SALVAR LOCALMENTE PRIMEIRO (status: pending_local)
    final pendingMessage = PendingMessage(
      msgId: messageId,
      to: toUserId,
      from: currentUserId,
      content: content,
      status: 'pending_local',
      createdAt: DateTime.now(),
    );

    await PendingMessagesStorage.savePendingMessage(pendingMessage);
    print('💾 Mensagem salva localmente: $messageId (status: pending_local)');

    // ✅ 2. ATUALIZAR UI IMEDIATAMENTE (mostrar mensagem com ícone 🕓)
    _updateChatOnMessageSent(toUserId, content);

    // ✅ 3. TENTAR ENVIAR AO SERVIDOR
    final okToSend = await canSendMessage();
    final isConnected = _channel != null && isWebSocketConnected();

    if (okToSend && isConnected) {
      // ✅ TEM INTERNET E SERVIDOR ONLINE -> TENTAR ENVIAR
      try {
        final message = {
          'type': 'message',
          'to': toUserId,
          'content': content,
          'message_id': messageId,
        };

        _sentMessageIds.add(messageId);
        _channel!.sink.add(json.encode(message));
        print('📤 Mensagem enviada ao servidor: $messageId');

        // ✅ Status será atualizado quando receber confirmação do servidor
        // (via handleIncomingMessage quando receber ACK)
      } catch (e) {
        print('❌ Erro ao enviar mensagem ao servidor: $e');
        // ✅ Mensagem permanece como pending_local para retry automático
        await PendingMessagesStorage.incrementRetryCount(messageId);
      }
    } else {
      // ✅ SEM INTERNET OU SERVIDOR OFFLINE -> MENSAGEM FICA PENDING
      print('⚠️ Sem conexão ou servidor offline -> Mensagem ficará pendente');
      print(
        '   Status: pending_local (será enviada automaticamente quando conexão voltar)',
      );
    }
  }

  // ✅ Atualizar status de mensagem quando receber confirmação do servidor
  static Future<void> updateMessageStatusFromServer(
    String messageId,
    String newStatus, {
    String? dbMessageId,
  }) async {
    final pendingMsg = await PendingMessagesStorage.getMessageById(messageId);

    if (pendingMsg != null) {
      // ✅ Atualizar status no storage local
      await PendingMessagesStorage.updateMessageStatus(
        messageId,
        newStatus,
        dbMessageId: dbMessageId,
      );

      // ✅ Se status for 'sent' ou superior, remover do sqflite após sincronização
      // (mensagem já foi sincronizada com sucesso)
      if (newStatus == 'sent' ||
          newStatus == 'delivered' ||
          newStatus == 'read') {
        // ✅ Aguardar um pouco para garantir que tudo foi processado
        Future.delayed(const Duration(seconds: 2), () async {
          await PendingMessagesStorage.deleteMessage(messageId);
          print('🧹 Mensagem sincronizada removida do sqflite: $messageId');
        });
      }

      print('🔄 Status atualizado: $messageId -> $newStatus');
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

  // ✅ DELETAR CONVERSA (Adicionado para permitir apagar da lista)
  static Future<void> deleteChat(String contactId) async {
    try {
      if (_chatContacts.containsKey(contactId)) {
        _chatContacts.remove(contactId);
        await _saveChatsToStorage();
        _chatListController.add(_getSortedChatList());
        print('🗑️ Conversa com $contactId removida da lista local');
      }
    } catch (e) {
      print('❌ Erro ao deletar conversa: $e');
    }
  }

  static Future<List<Map<String, dynamic>>> loadChatHistory(
    String contactUserId,
  ) async {
    try {
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (currentUserId == null) {
        print('❌ User ID não encontrado no SecureStorage');
        return await loadLocalChatHistory('unknown', contactUserId);
      }

      //final url = Uri.parse(
      //  'http://10.0.2.2:4000/api/messages/history/$currentUserId/$contactUserId',
      //);

      final url = Uri.parse(
        'http://192.168.100.35:4000/api/messages/history/$currentUserId/$contactUserId',
      );

      print('📨 Carregando histórico: $currentUserId -> $contactUserId');

      final accessToken = await _secureStorage.read(key: 'access_token');
      final headers = {
        'Content-Type': 'application/json',
        if (accessToken != null) 'Authorization': 'Bearer $accessToken',
      };

      try {
        final response = await http
            .get(url, headers: headers)
            .timeout(Duration(seconds: 5));
        print('📡 Response status: ${response.statusCode}');

        if (response.statusCode == 200) {
          final data = json.decode(response.body);
          final messages = (data['messages'] as List<dynamic>? ?? []);
          print('✅ Histórico carregado: ${messages.length} mensagens');

          // ✅ Salvar cópia local para uso offline
          await _saveChatHistoryToStorage(
            currentUserId,
            contactUserId,
            messages,
          );

          return messages.cast<Map<String, dynamic>>();
        } else {
          print('❌ Erro ao carregar histórico: ${response.statusCode}');
          return await loadLocalChatHistory(currentUserId, contactUserId);
        }
      } on TimeoutException catch (_) {
        print(
          '⚠️ Servidor indisponível (timeout) - carregando histórico local',
        );
        return await loadLocalChatHistory(currentUserId, contactUserId);
      }
    } catch (e) {
      print('❌ Erro loadChatHistory: $e');

      // Fallback: tentar histórico local em caso de erro (inclui sem internet)
      try {
        final currentUserId = await _secureStorage.read(key: 'user_id');
        return await loadLocalChatHistory(
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

  // ✅ Agora PÚBLICO para acesso direto
  static Future<List<Map<String, dynamic>>> loadLocalChatHistory(
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
      // Silenciar log de carregamento local para reduzir spam
      // print('📂 Histórico local carregado: ${data.length} mensagens...');
      return data.cast<Map<String, dynamic>>();
    } catch (e) {
      print('❌ Erro ao carregar histórico local: $e');
      return [];
    }
  }

  // ✅ NOVO: Salvar mensagem no histórico local (para persistência)
  static Future<void> saveMessageToLocalHistory(
    String meId,
    String contactId,
    Map<String, dynamic> message,
  ) async {
    try {
      final key = _historyStorageKey(meId, contactId);
      final existing = await loadLocalChatHistory(meId, contactId);

      // ✅ Verificar se mensagem já existe (evitar duplicatas)
      final messageId = message['message_id']?.toString();
      final exists = existing.any(
        (msg) =>
            (msg['message_id']?.toString() == messageId) ||
            (msg['id']?.toString() == messageId),
      );

      if (!exists) {
        existing.add(message);
        await _saveChatHistoryToStorage(meId, contactId, existing);
        print('💾 Mensagem salva no histórico local: $messageId');
      }
    } catch (e) {
      print('❌ Erro ao salvar mensagem no histórico local: $e');
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
      //final url = Uri.parse(
      //  'http://10.0.2.2:4000/api/messages/mark_read/$meId/$contactId',
      //);
      final url = Uri.parse(
        'http://192.168.100.35:4000/api/messages/mark_read/$meId/$contactId',
      );

      final headers = {
        'Content-Type': 'application/json',
        'Authorization': 'Bearer $token',
      };
      final res = await http
          .post(url, headers: headers)
          .timeout(Duration(seconds: 5));
      print('📡 markMessagesRead response: ${res.statusCode}');
    } on TimeoutException catch (_) {
      // ✅ Timeout esperado em modo offline - silenciar
      print(
        '⚠️ Servidor offline - mensagens não marcadas como lidas no servidor',
      );
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
    // Cancelar timer anterior se existir
    _saveDebounceTimer?.cancel();

    // Agendar salvamento após 500ms de inatividade
    _saveDebounceTimer = Timer(Duration(milliseconds: 500), () async {
      try {
        final chatsMap = {};
        _chatContacts.forEach((key, value) {
          chatsMap[key] = value.toMap();
        });
        final jsonData = json.encode(chatsMap);
        await _secureStorage.write(key: 'chat_contacts', value: jsonData);
        print('💾 Chats salvos no storage: ${_chatContacts.length} chats');

        // Aproveitar e salvar o cache de telefones também
        _saveContactCacheToStorage();
      } catch (e) {
        print('❌ Erro ao salvar chats: $e');
      }
    });
  }

  static Future<void> loadLocalChats() async {
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

      // ✅ Carregar também o cache de IDs -> Phones
      await _loadContactCacheFromStorage();
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

      //final url = Uri.parse('http://10.0.2.2:4000/api/presence/$userId');
      final url = Uri.parse('http://192.168.100.35:4000/api/presence/$userId');

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
    } on TimeoutException catch (_) {
      // ✅ Timeout esperado em modo offline - silenciar
      // print('⚠️ Timeout ao obter presença (offline)');
      return {'status': 'offline', 'last_seen': null};
    } catch (e, stackTrace) {
      print('❌ Erro ao obter presença: $e');
      print('📚 Stack trace: $stackTrace');
      return {'status': 'offline', 'last_seen': null};
    }
  }

  // ✅ ATUALIZAR PRESENÇA DE UM USUÁRIO ESPECÍFICO (usar quando reconectar)
  static Future<void> refreshUserPresence(String userId) async {
    try {
      print('🔄 Atualizando presença do usuário: $userId');

      final presence = await getUserPresence(userId);
      if (presence != null) {
        final status = presence['status']?.toString() ?? 'offline';
        final nowTs = DateTime.now().millisecondsSinceEpoch ~/ 1000;

        // Atualizar cache e notificar listeners
        _userPresenceStatus[userId] = status;
        _presenceTimestamps[userId] = nowTs;

        _presenceController.add({
          'user_id': userId,
          'status': status,
          'timestamp': nowTs,
        });

        print('✅ Presença atualizada: $userId -> $status');
      }
    } catch (e) {
      print('❌ Erro ao atualizar presença: $e');
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

  static void _updateChatContentOnlyWithDeletedMessageIfLast(
    Map<String, dynamic> message,
  ) async {
    try {
      print(
        '🔍 DEBUG _updateChatContentOnlyWithDeletedMessageIfLast: Verificando se é última mensagem',
      );

      // ✅ VERIFICAR SE É A ÚLTIMA MENSAGEM ANTES DE ATUALIZAR CHAT LIST
      final messageId = message['message_id']?.toString();
      final fromUserId =
          message['from']?.toString() ?? message['sender_id']?.toString();
      final toUserId =
          message['to']?.toString() ?? message['receiver_id']?.toString();
      var currentUserId = await _secureStorage.read(key: 'user_id');

      if (currentUserId == null ||
          fromUserId == null ||
          toUserId == null ||
          messageId == null) {
        print('❌ Dados insuficientes para verificar última mensagem');
        return;
      }

      // Verificar se é a última mensagem no chat list atual
      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;

        // ✅ LÓGICA CORRETA: Verificar se a última mensagem no chat list é a mesma que foi deletada
        // Para isso, precisamos comparar o ID da mensagem atual no chat list
        // Se não conseguirmos determinar, não atualizamos para evitar problemas
        print(
          '🔍 DEBUG: Verificando se mensagem $messageId é a última do chat',
        );
        print('   🔍 Última mensagem no chat list: ${existing.lastMessage}');
        print('   🔍 Contém ⊗? ${existing.lastMessage.contains('⊗')}');

        // Se a última mensagem não contém ⊗, significa que é uma mensagem normal
        // Mas ainda assim não sabemos se é a mesma mensagem
        // Por segurança, só atualizamos se tivermos certeza
        if (!existing.lastMessage.contains('⊗')) {
          print(
            '🚫 Última mensagem é normal, mas não temos certeza se é a mesma - NÃO ATUALIZANDO',
          );
          print('   🔍 Para evitar problemas, não atualizamos chat list');
          return;
        }

        // Se a última mensagem já contém ⊗, significa que já foi deletada
        // Não atualizamos novamente
        print('🚫 Última mensagem já é deletada - NÃO ATUALIZANDO chat list');
        print(
          '   🔍 Mensagem atual: $messageId, última mensagem já foi processada',
        );
      } else {
        print('🚫 Chat não encontrado na lista - NÃO ATUALIZANDO');
      }
    } catch (e) {
      print('❌ Erro ao verificar última mensagem: $e');
      // Em caso de erro, não atualizar para evitar problemas
    }
  }

  static void _updateChatContentOnlyWithDeletedMessage(
    Map<String, dynamic> message,
  ) async {
    try {
      print(
        '🔍 DEBUG _updateChatContentOnlyWithDeletedMessage: Mensagem completa=$message',
      );
      print('🔍 DEBUG Content recebido: ${message['content']}');
      print('🔍 DEBUG DeletedBy recebido: ${message['deleted_by']}');
      // ✅ TENTAR from/to PRIMEIRO (chat_list_update), DEPOIS sender_id/receiver_id (message_deleted)
      String? fromUserId =
          message['from']?.toString() ?? message['sender_id']?.toString();
      String? toUserId =
          message['to']?.toString() ?? message['receiver_id']?.toString();
      var currentUserId = await _secureStorage.read(key: 'user_id');

      print(
        '🔍 DEBUG: fromUserId=$fromUserId, toUserId=$toUserId, currentUserId=$currentUserId',
      );

      if (currentUserId == null || fromUserId == null || toUserId == null) {
        print('❌ Dados insuficientes para atualizar conteúdo do chat');
        return;
      }

      String contactId = fromUserId == currentUserId ? toUserId : fromUserId;

      if (_chatContacts.containsKey(contactId)) {
        final existing = _chatContacts[contactId]!;
        print('🗑️ ATUALIZANDO CHAT LIST COM MENSAGEM DELETADA:');
        print('   - Chat: ${existing.name}');

        // ✅ PERSONALIZAR MENSAGEM DELETADA
        final deletedText = message['deleted_by']?.toString() == currentUserId
            ? '⊗ Eliminou esta mensagem'
            : '⊗ Esta mensagem foi apagada';

        _chatContacts[contactId] = existing.copyWith(
          name: existing.name,
          phoneNumber: existing.phoneNumber,
          photo: existing.photo,
          lastMessageTime: existing.lastMessageTime, // ✅ PRESERVAR TIMESTAMP!
          lastMessage: deletedText, // ✅ USAR MENSAGEM PERSONALIZADA
          unreadCount: existing.unreadCount, // ✅ MANTER UNREAD
          lastMessageIsReply: false,
        );
        print('   ✅ Chat list atualizado com mensagem deletada: $deletedText');
        print('   🔍 DEBUG: _chatContacts.length=${_chatContacts.length}');
        print(
          '   🔍 DEBUG: contactId=$contactId existe=${_chatContacts.containsKey(contactId)}',
        );

        // ✅ FORÇAR ATUALIZAÇÃO IMEDIATA
        _chatListController.add(_chatContacts.values.toList());
        _saveChatsToStorage();

        print(
          '   🔍 DEBUG: Chat list controller atualizado com ${_chatContacts.length} chats',
        );
      }

      // ✅ ATUALIZAR FRONTEND SEM REORDENAR
      _chatListController.add(_chatContacts.values.toList());
      _saveChatsToStorage();
    } catch (e) {
      print('❌ Erro ao atualizar conteúdo do chat: $e');
    }
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
}
