import 'dart:async';
import 'dart:convert';
import 'dart:io';
import 'package:flutter/material.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:uuid/uuid.dart';
import 'package:image_picker/image_picker.dart';
import 'package:web_socket_channel/web_socket_channel.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'chat_model.dart';
import 'chat_service.dart';
import 'message_operations_service.dart';
import 'auth_service.dart';
import 'notification_service.dart';
import 'app_theme.dart';
import 'dart:math';

class ChatMessage {
  final String id;
  final String text;
  final bool isMe;
  final DateTime timestamp;
  final String status; // 'sent', 'delivered', 'read'
  final String? replyToId; // ID da mensagem respondida
  final String? replyToText; // Texto da mensagem respondida
  final String? replyToSenderName; // Nome de quem enviou a mensagem respondida
  final String? replyToSenderId; // ID de quem enviou a mensagem respondida

  ChatMessage({
    required this.id,
    required this.text,
    required this.isMe,
    required this.timestamp,
    required this.status,
    this.replyToId,
    this.replyToText,
    this.replyToSenderName,
    this.replyToSenderId,
  });
}

class MessageGroup {
  final DateTime date;
  final List<ChatMessage> messages;

  MessageGroup({required this.date, required this.messages});
}

class ChatPage extends StatefulWidget {
  final ChatContact contact;
  final String remoteUserId;

  const ChatPage({Key? key, required this.contact, required this.remoteUserId})
    : super(key: key);

  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> with WidgetsBindingObserver {
  final TextEditingController _messageController = TextEditingController();
  final List<ChatMessage> _messages = [];
  final ScrollController _scrollController = ScrollController();
  final ImagePicker _imagePicker = ImagePicker();

  String? _currentUserId;
  bool _isConnected = false;
  bool _isLoadingHistory = false;
  StreamSubscription? _messageSubscription;
  StreamSubscription? _presenceSubscription;
  final Set<String> _pendingMessageIds = {};
  final Map<String, String> _pendingStatusUpdates = {};
  final Uuid _uuid = Uuid();
  bool _hasMarkedAsRead = false;
  Timer? _markAsReadTimer;
  bool _isAppInBackground = false; // Nova variável para controlar background

  // Controles para áudio e emojis
  bool _isRecording = false;
  bool _showEmojiPicker = false;

  // Controles para edição e seleção de mensagens
  String? _selectedMessageId;
  String? _editingMessageId;
  TextEditingController _editController = TextEditingController();

  // Lista de emojis comuns
  static const List<String> _commonEmojis = [
    '😀',
    '😃',
    '😄',
    '😁',
    '😅',
    '😂',
    '🤣',
    '😊',
    '😇',
    '🙂',
    '🙃',
    '😉',
    '😌',
    '😍',
    '🥰',
    '😘',
    '😗',
    '😙',
    '😚',
    '😋',
    '😛',
    '😜',
    '🤪',
    '😝',
    '🤑',
    '🤗',
    '🤭',
    '🤫',
    '🤥',
    '😶',
    '😐',
    '😑',
    '😬',
    '🙄',
    '😯',
    '😦',
    '😧',
    '😮',
    '😲',
    '🥱',
    '😴',
    '🤤',
    '😪',
    '😵',
    '🤐',
    '🥴',
    '🤢',
    '🤮',
    '🤧',
    '😷',
    '🤒',
    '🤕',
    '🤡',
    '👍',
    '👎',
    '👌',
    '✌️',
    '🤞',
    '🤟',
    '🤘',
    '🤙',
    '👈',
    '👉',
    '👆',
    '🖕',
    '👇',
    '☝️',
    '✋',
    '🤚',
    '🖐',
    '🖖',
    '👋',
    '💪',
    '🦾',
    '🦿',
    '🦶',
    '🦵',
    '🦴',
    '🦷',
    '❤️',
    '🧡',
    '💛',
    '💚',
    '💙',
    '💜',
    '🖤',
    '🤍',
    '🤎',
    '💔',
    '❣️',
    '💕',
    '💞',
    '💓',
    '💗',
    '💖',
    '💘',
    '💝',
    '🎉',
    '🎊',
    '🎈',
    '🎁',
    '🎀',
    '🎗',
    '🎟',
    '🎫',
    '🎖',
    '🏆',
    '🥇',
    '🥈',
    '🥉',
    '⚽',
    '🏀',
    '🏈',
    '⚾',
    '🎾',
    '🎱',
    '🏐',
    '🏓',
    '🥏',
    '🥅',
    '🎳',
    '🏏',
    '🎯',
    '🎪',
    '🎨',
    '🖌',
    '🖍',
    '📝',
    '✏️',
    '✒️',
    '🖊',
    '🖋',
    '📎',
    '📌',
    '📍',
  ];

  // Status de presença do contato
  String _contactPresenceStatus = 'offline'; // 'online', 'offline'
  Timer? _presenceOnlineTimer;
  Timer? _presenceOfflineTimer;

  @override
  void initState() {
    super.initState();
    // Registrar observer para detectar background
    WidgetsBinding.instance.addObserver(this);

    // Informar ao ChatService qual chat está ativo (para controle de unread)
    ChatService.setActiveChat(widget.remoteUserId);
    _initializeChat();

    // Marcar como lido ao abrir o chat
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _markAsReadOnOpen();
    });
  }

  // Marcar como lido ao abrir (com pequeno delay)
  void _markAsReadOnOpen() {
    if (_hasMarkedAsRead) return;

    print('🚪 Chat aberto - agendando marcação como lido...');

    // ✅ Pequeno delay para garantir que tudo foi carregado
    _markAsReadTimer = Timer(Duration(milliseconds: 500), () {
      if (!_hasMarkedAsRead && mounted) {
        print('Marcando chat como lido ao abrir');
        _markChatAsRead();
        _hasMarkedAsRead = true;
      }
    });
  }

  @override
  void dispose() {
    // Remover observer
    WidgetsBinding.instance.removeObserver(this);

    _markAsReadTimer?.cancel();
    _presenceOnlineTimer?.cancel();
    _presenceOfflineTimer?.cancel();

    // GARANTIR que marca como lido se ainda não marcou
    if (!_hasMarkedAsRead && mounted) {
      print('🚪 Saindo do chat - marcando como lido finalmente');
      _markChatAsRead();
    }

    // Limpar chat ativo
    ChatService.clearActiveChat(widget.remoteUserId);

    _messageSubscription?.cancel();
    _presenceSubscription?.cancel();
    _pendingMessageIds.clear();
    super.dispose();
  }

  // Detectar mudanças no ciclo de vida da app
  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    print('🔄 ChatPage Lifecycle State changed to: $state');

    switch (state) {
      case AppLifecycleState.paused:
        print('🌑 App foi para background - resetando marcação de lido');
        _isAppInBackground = true;
        // Resetar para permitir marcar como lido quando voltar
        _hasMarkedAsRead = false;
        break;
      case AppLifecycleState.resumed:
        print('☀️ App voltou para foreground - marcando mensagens como lidas');
        _isAppInBackground = false;
        // Se voltou para foreground, marcar mensagens como lidas imediatamente
        if (!_hasMarkedAsRead) {
          print('📖 Marcando mensagens como lidas ao voltar para foreground');
          _markChatAsRead();
          _hasMarkedAsRead = true;
        }
        break;
      case AppLifecycleState.detached:
        print('💀 App sendo destruída');
        break;
      default:
        break;
    }
  }

  // Carregar status de presença do contacto
  Future<void> _loadContactPresence() async {
    try {
      print('🔍 Buscando presença para: ${widget.remoteUserId}');
      final presence = await ChatService.getUserPresence(widget.remoteUserId);
      print('📊 Presença recebida: $presence');

      if (presence != null && mounted) {
        setState(() {
          _contactPresenceStatus = presence['status'] ?? 'offline';
        });
        print('✅ Status atualizado: $_contactPresenceStatus');
      } else {
        // Se não conseguir buscar, definir como offline
        if (mounted) {
          setState(() {
            _contactPresenceStatus = 'offline';
          });
          print(
            '⚠️ Não foi possível carregar presença, definindo como offline',
          );
        }
      }
    } catch (e) {
      print('❌ Erro ao carregar presença: $e');
      // Em caso de erro, definir como offline
      if (mounted) {
        setState(() {
          _contactPresenceStatus = 'offline';
        });
      }
    }
  }

  // Formatar status para exibição
  String _getPresenceText() {
    if (_contactPresenceStatus == 'online') {
      return 'online';
    }

    // REQUISITO: quando offline, NÃO mostrar nada (campo vazio)
    // Isso significa "offline" de forma silenciosa.
    return '';
  }

  // MELHORADO: Marcar como lido com verificação
  void _markChatAsRead() {
    if (_hasMarkedAsRead) {
      print('⏳ Chat já foi marcado como lido nesta sessão');
      return;
    }

    print('📖 Marcando chat como lido');
    // Usar versão IMEDIATA (sem cooldown) quando o usuário abre o chat
    ChatService.markChatAsReadImmediate(widget.remoteUserId);
    ChatService.markMessagesRead(widget.remoteUserId);
    _hasMarkedAsRead = true;
  }

  void _initializeChat() async {
    try {
      print('🚀 Inicializando chat dinâmico...');

      _currentUserId = await AuthService.getCurrentUserId();
      print('   - Current User ID: $_currentUserId');

      if (_currentUserId == null) {
        print('❌ Não foi possível obter o user_id atual');
        return;
      }

      await _setupRealChat();
      await _loadChatHistory();
    } catch (e) {
      print('❌ Erro na inicialização do chat: $e');
    }
  }

  Future<void> _setupRealChat() async {
    final connected = await ChatService.connect();

    if (connected && mounted) {
      setState(() {
        _isConnected = true;
      });

      _messageSubscription = ChatService.messageStream.listen((message) {
        print('💬 Mensagem recebida: $message');
        _handleIncomingMessage(message);
      });

      // ESCUTAR EVENTOS DE PRESENÇA (com delay de 2s para aparecer/sumir)
      _presenceSubscription = ChatService.presenceStream.listen((presence) {
        final userId = presence['user_id']?.toString();
        final status = presence['status']?.toString();

        if (userId == widget.remoteUserId && status != null && mounted) {
          print('📡 Evento de presença recebido: $userId -> $status');

          // Cancelar timers anteriores para evitar "piscar"
          _presenceOnlineTimer?.cancel();
          _presenceOfflineTimer?.cancel();

          if (status == 'online') {
            // Esperar 2 segundos antes de mostrar "online"
            _presenceOnlineTimer = Timer(const Duration(seconds: 2), () {
              if (!mounted) return;
              setState(() {
                _contactPresenceStatus = 'online';
              });
              print('✅ Presença aplicada (ONLINE) após delay');
            });
          } else if (status == 'offline') {
            // Esperar 2 segundos antes de remover o "online"
            _presenceOfflineTimer = Timer(const Duration(seconds: 2), () async {
              if (!mounted) return;

              setState(() {
                _contactPresenceStatus = 'offline';
              });

              await _loadContactPresence();
              print('✅ Presença aplicada (OFFLINE) após delay');
            });
          }
        }
      });

      // BUSCAR STATUS INICIAL COM DELAY DE 2s TAMBÉM
      Future.delayed(const Duration(seconds: 2), () {
        if (mounted) {
          _loadContactPresence();
        }
      });
    } else {
      print('❌ Falha na conexão WebSocket');
    }
  }

  void _debugPrintMessage(String prefix, Map<String, dynamic> message) {
    print('$prefix:');
    print('   type: ${message['type']}');
    print('   from: ${message['from']}');
    print('   to: ${message['to']}');
    print('   content: ${message['content']}');
    print('   message_id: ${message['message_id']}');
    print('   db_message_id: ${message['db_message_id']}');
    print('   reply_to_id: ${message['reply_to_id']}');
    print('   reply_to_text: ${message['reply_to_text']}');
    print('   reply_to_sender_name: ${message['reply_to_sender_name']}');
    print('   status: ${message['status']}');
  }

  // ======================
  // MELHORIA NO _handleIncomingMessage()
  // ======================
  void _handleIncomingMessage(Map<String, dynamic> message) {
    final type = message['type']?.toString();

    switch (type) {
      case 'message_edited':
        _handleEditedMessage(message);
        return;
      case 'message_deleted':
        _handleDeletedMessage(message);
        return;
      //case 'message_reply':
      //  _handleReplyMessage(message);
      //  return;
      default:
        break;
    }

    // ✅ TRATAMENTO DE STATUS
    if (type == 'message_delivered' || type == 'message_read') {
      final messageId = message['message_id']?.toString();
      final dbMessageId = message['db_message_id']?.toString();

      if (messageId != null) {
        final newStatus = type == 'message_delivered' ? 'delivered' : 'read';

        // Tenta encontrar por UUID (messageId) OU pelo ID de banco (dbMessageId)
        // Isso resolve o problema de incompatibilidade entre UUID local e ID do banco
        final idx = _messages.indexWhere((m) {
          final matchesUuid = m.id == messageId;
          final matchesDbId = dbMessageId != null && m.id == dbMessageId;
          // Também verificar se o messageId do evento já é o ID numérico (caso do read)
          final matchesIdDirectly = m.id == messageId;

          return (matchesUuid || matchesDbId || matchesIdDirectly) && m.isMe;
        });

        if (idx >= 0 && mounted) {
          final oldMsg = _messages[idx];

          // Evitar downgrade de status (ex: read -> delivered)
          if (oldMsg.status == 'read' && newStatus == 'delivered') {
            print(
              '⚠️ Ignorando status anterior ($newStatus) pois já está lida',
            );
            return;
          }

          print('✅ Atualizando mensagem ${oldMsg.id} para $newStatus');

          setState(() {
            // ✅ CRÍTICO: Se recebermos o ID do banco (dbMessageId),
            // atualizamos o ID local para garantir que eventos futuros (ex: read)
            // que usam o ID do banco consigam encontrar a mensagem.
            final finalId = dbMessageId ?? oldMsg.id;

            _messages[idx] = ChatMessage(
              id: finalId,
              text: oldMsg.text,
              isMe: oldMsg.isMe,
              timestamp: oldMsg.timestamp,
              status: newStatus,
              // ✅ preservar dados de reply
              replyToId: oldMsg.replyToId,
              replyToText: oldMsg.replyToText,
              replyToSenderName: oldMsg.replyToSenderName,
              replyToSenderId: oldMsg.replyToSenderId,
            );
          });
        } else if (idx == -1 && dbMessageId != null && mounted) {
          // ✅ FALLBACK HEURÍSTICO: Se não encontrou pelo ID (Ack perdido ou race condition),
          // tenta encontrar uma mensagem "órfã" (minha, enviada, com UUID) para associar.
          print(
            '⚠️ Mensagem não encontrada por ID direto. Tentando pareamento heurístico...',
          );

          // Busca a primeira mensagem minha, com status 'sent' e ID não numérico (UUID)
          final candidateIdx = _messages.indexWhere(
            (m) =>
                m.isMe &&
                m.status == 'sent' &&
                int.tryParse(m.id) == null, // Assume que UUID não é numérico
          );

          if (candidateIdx >= 0) {
            final oldMsg = _messages[candidateIdx];
            print(
              '✅ Pareamento heurístico SUCESSO! Associando entrega $dbMessageId à mensagem local ${oldMsg.id}',
            );

            setState(() {
              _messages[candidateIdx] = ChatMessage(
                id: dbMessageId, // SWAP FORÇADO AGORA
                text: oldMsg.text,
                isMe: oldMsg.isMe,
                timestamp: oldMsg.timestamp,
                status: newStatus,
                // ✅ preservar dados de reply
                replyToId: oldMsg.replyToId,
                replyToText: oldMsg.replyToText,
                replyToSenderName: oldMsg.replyToSenderName,
                replyToSenderId: oldMsg.replyToSenderId,
              );
            });

            // Limpa pendências se houver
            _pendingMessageIds.remove(oldMsg.id);
          } else {
            print(
              '⚠️ Mensagem não encontrada para atualização de status (nem heurística). Armazenando pendência.',
            );
            print(
              '   IDs buscados: messageId=$messageId, dbMessageId=$dbMessageId',
            );
            _pendingStatusUpdates[dbMessageId] = newStatus;
            print('   📌 Status "$newStatus" guardado para ID $dbMessageId');
          }
        } else {
          print(
            '⚠️ Mensagem não encontrada para atualização de status. Armazenando pendência.',
          );
          print(
            '   IDs buscados: messageId=$messageId, dbMessageId=$dbMessageId',
          );
          if (dbMessageId != null) {
            _pendingStatusUpdates[dbMessageId] = newStatus;
            print('   📌 Status "$newStatus" guardado para ID $dbMessageId');
          }
        }
        _pendingMessageIds.remove(messageId);
      }
      return;
    }

    final fromUserId = message['from']?.toString();
    final toUserId = message['to']?.toString();
    final messageId = message['message_id']?.toString();
    final content = message['content']?.toString() ?? '';

    final isMessageForThisChat =
        (fromUserId == widget.remoteUserId && toUserId == _currentUserId) ||
        (fromUserId == _currentUserId && toUserId == widget.remoteUserId);

    if (isMessageForThisChat && mounted) {
      print('📨 Mensagem recebida: $message');

      // ✅ DETECTAR SE É UMA RESPOSTA
      final isReply = message['reply_to_id'] != null;
      if (isReply) {
        print('🔍 MENSAGEM É UMA RESPOSTA!');
        print('   reply_to_id: ${message['reply_to_id']}');
        print('   reply_to_text: ${message['reply_to_text']}');
        print('   reply_to_sender_name: ${message['reply_to_sender_name']}');
      }

      final isFromMe = fromUserId == _currentUserId;
      final dbMessageId = message['db_message_id']?.toString();

      // ✅ CORREÇÃO: VERIFICAR SE É UM SWAP DE REPLY
      if (isFromMe && messageId != null && dbMessageId != null) {
        final idx = _messages.indexWhere((m) => m.id == messageId);
        if (idx >= 0) {
          print('🔄 SWAP DETECTADO PARA REPLY: $messageId -> $dbMessageId');

          setState(() {
            final old = _messages[idx];
            // ❗ Não fazer downgrade de status: se já está delivered/read,
            // não voltar para 'sent' por causa do echo.
            final incomingStatus = message['status']?.toString() ?? old.status;
            final finalStatus =
                (old.status == 'read' || old.status == 'delivered') &&
                    incomingStatus == 'sent'
                ? old.status
                : incomingStatus;

            _messages[idx] = ChatMessage(
              id: dbMessageId,
              text: old.text,
              isMe: old.isMe,
              timestamp: old.timestamp,
              status: finalStatus,
              // ✅ PRESERVAR INFORMAÇÕES DE REPLY
              replyToId: old.replyToId,
              replyToText: old.replyToText,
              replyToSenderName: old.replyToSenderName,
              replyToSenderId: old.replyToSenderId,
            );
          });
          _pendingMessageIds.remove(messageId);
          return;
        }
      }

      // ✅ SWAP HEURÍSTICO PARA REPLIES (quando o servidor envia só o ID real)
      if (isFromMe &&
          dbMessageId != null &&
          (message['reply_to_id'] != null ||
              message['reply_to_text'] != null)) {
        final pendingIdx = _messages.indexWhere(
          (m) =>
              m.isMe &&
              m.status == 'sent' &&
              m.replyToId == message['reply_to_id']?.toString() &&
              m.text == content,
        );

        if (pendingIdx >= 0) {
          final old = _messages[pendingIdx];
          print('🔄 SWAP HEURÍSTICO DE REPLY: ${old.id} -> $dbMessageId');
          setState(() {
            // ❗ Também não fazer downgrade de status aqui.
            final incomingStatus = message['status']?.toString() ?? old.status;
            final finalStatus =
                (old.status == 'read' || old.status == 'delivered') &&
                    incomingStatus == 'sent'
                ? old.status
                : incomingStatus;

            _messages[pendingIdx] = ChatMessage(
              id: dbMessageId,
              text: old.text,
              isMe: old.isMe,
              timestamp: old.timestamp,
              status: finalStatus,
              replyToId: old.replyToId,
              replyToText: old.replyToText,
              replyToSenderName: old.replyToSenderName,
              replyToSenderId: old.replyToSenderId,
            );
          });
          _pendingMessageIds.remove(old.id);
          return;
        }
      }

      // ✅ VERIFICAÇÃO DE DUPLICAÇÃO MELHORADA
      final isPendingMessage = _pendingMessageIds.contains(messageId ?? '');
      final existingMessage = _messages.any(
        (msg) =>
            (messageId != null && msg.id == messageId) ||
            (dbMessageId != null && msg.id == dbMessageId),
      );

      if (!existingMessage && !isPendingMessage) {
        print('✅ ADICIONANDO MENSAGEM NOVA');

        final serverTimestamp = _parseRealTimeMessageTimestamp(message);
        final finalId = dbMessageId ?? messageId ?? _uuid.v4();

        setState(() {
          _messages.add(
            ChatMessage(
              id: finalId,
              text: content,
              isMe: isFromMe,
              timestamp: serverTimestamp,
              status: message['status']?.toString() ?? 'sent',
              // ✅ INFORMAÇÕES DE REPLY (SE HOUVER)
              replyToId: message['reply_to_id']?.toString(),
              replyToText: message['reply_to_text']?.toString(),
              replyToSenderName: message['reply_to_sender_name']?.toString(),
              replyToSenderId: message['reply_to_sender_id']?.toString(),
            ),
          );

          _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
        });

        _scrollToBottom();

        if (isFromMe && messageId != null) {
          _pendingMessageIds.remove(messageId);
        } else if (!isFromMe) {
          if (!_isAppInBackground) {
            print('📖 Mensagem recebida - marcando como lida');
            ChatService.markChatAsReadImmediate(widget.remoteUserId);
            ChatService.markMessagesRead(widget.remoteUserId);
          } else {
            print('🌑 Mensagem em background - enviando notificação');
            _sendNewMessageNotification(content);
          }
        }
      }
    }
  }

  // NOVOS HANDLERS PARA WEBSOCKET

  // Handler para mensagens de resposta recebidas via WebSocket
  void _handleReplyMessage(Map<String, dynamic> message) {
    final replyContent = message['content']?.toString();
    final senderId = message['sender_id']?.toString();
    final originalId = message['original_message_id']?.toString();

    if (replyContent != null && senderId != null) {
      final isFromMe = senderId == _currentUserId;
      final serverTimestamp = _parseRealTimeMessageTimestamp(message);

      // ✅ ENCONTRAR MENSAGEM ORIGINAL PARA OBTER TEXTO E NOME
      String? originalText;
      String? originalSenderName;

      try {
        final originalMessage = _messages.firstWhere(
          (msg) => msg.id == originalId,
          orElse: () => ChatMessage(
            id: originalId ?? '',
            text: 'Mensagem não encontrada',
            isMe: false,
            timestamp: DateTime.now(),
            status: 'sent',
          ),
        );

        originalText = originalMessage.text;
        originalSenderName = originalMessage.isMe ? 'Eu' : widget.contact.name;
      } catch (e) {
        originalText = 'Mensagem não encontrada';
        originalSenderName = 'Desconhecido';
      }

      final replyMessage = ChatMessage(
        id: message['message_id']?.toString() ?? _uuid.v4(),
        text: replyContent,
        isMe: isFromMe,
        timestamp: serverTimestamp,
        status: message['status']?.toString() ?? 'sent',
        // ✅ INFORMAÇÕES DA RESPOSTA
        replyToId: originalId,
        replyToText: originalText,
        replyToSenderName: originalSenderName,
        replyToSenderId: message['reply_to_sender_id']?.toString(),
      );

      setState(() {
        _messages.add(replyMessage);
        _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
      });

      _scrollToBottom();
    }
  }

  // Handler para mensagens deletadas recebidas via WebSocket
  void _handleDeletedMessage(Map<String, dynamic> message) {
    final messageId = message['message_id']?.toString();

    if (messageId != null) {
      setState(() {
        _messages.removeWhere((msg) => msg.id == messageId);
      });
    }
  }

  // Handler para mensagens editadas recebidas via WebSocket
  void _handleEditedMessage(Map<String, dynamic> message) {
    final messageId = message['message_id']?.toString();
    final newContent = message['content']?.toString();

    if (messageId != null && newContent != null) {
      print(
        '✏️ Recebida notificação de edição: messageId=$messageId, newContent=$newContent',
      );

      setState(() {
        final messageIndex = _messages.indexWhere((msg) => msg.id == messageId);
        if (messageIndex != -1) {
          final oldMessage = _messages[messageIndex];
          _messages[messageIndex] = ChatMessage(
            id: oldMessage.id,
            text: newContent,
            isMe: oldMessage.isMe,
            timestamp: oldMessage.timestamp,
            status: 'edited', // ✅ MARCAR COMO EDITADA
            // ✅ PRESERVAR DADOS DE REPLY
            replyToId: oldMessage.replyToId,
            replyToText: oldMessage.replyToText,
            replyToSenderName: oldMessage.replyToSenderName,
            replyToSenderId: oldMessage.replyToSenderId,
          );
          print('✅ Mensagem ${messageId} atualizada em tempo real');
        }
      });
    }
  }

  // 🔔 ENVIAR NOTIFICAÇÃO DE NOVA MENSAGEM
  void _sendNewMessageNotification(String messageContent) async {
    try {
      final senderName = _getContactName();

      await NotificationService().showNewMessageNotification(
        senderName: senderName,
        messageContent: messageContent,
        chatId: widget.remoteUserId,
      );

      print('🔔 Notificação enviada para: $senderName');
    } catch (e) {
      print('❌ Erro ao enviar notificação: $e');
    }
  }

  String _getContactName() {
    return widget.contact.name.isNotEmpty
        ? widget.contact.name
        : (_getContactPhone() ?? 'Sem nome');
  }

  String? _getContactPhone() {
    return widget.contact.phoneNumber?.isNotEmpty == true
        ? widget.contact.phoneNumber
        : null;
  }

  DateTime _parseRealTimeMessageTimestamp(Map<String, dynamic> message) {
    try {
      final timestamp = message['timestamp'];
      print('🔍 Parseando timestamp em tempo real: $timestamp');

      if (timestamp is int) {
        if (timestamp > 1000000000000) {
          return DateTime.fromMillisecondsSinceEpoch(timestamp);
        } else {
          return DateTime.fromMillisecondsSinceEpoch(timestamp * 1000);
        }
      } else if (timestamp is String) {
        return DateTime.parse(timestamp);
      }
    } catch (e) {
      print('❌ Erro ao parsear timestamp em tempo real: $e');
    }

    return DateTime.now();
  }

  Future<void> _loadChatHistory() async {
    if (_isLoadingHistory || _currentUserId == null) return;

    setState(() => _isLoadingHistory = true);

    try {
      print('📜 Carregando histórico dinâmico...');
      final history = await ChatService.loadChatHistory(widget.remoteUserId);

      if (mounted && history.isNotEmpty) {
        setState(() {
          _messages.addAll(
            history.map((msg) {
              final serverTimestamp = _parseMessageTimestamp(msg);

              return ChatMessage(
                id: (msg['message_id'] ?? msg['id'] ?? _uuid.v4()).toString(),
                text: msg['content'] ?? '',
                isMe: _isMessageFromMe(msg),
                timestamp: serverTimestamp,
                status: (msg['status']?.toString() ?? 'sent'),
                // ✅ INFORMAÇÕES DE RESPOSTA DO HISTÓRICO
                replyToId: msg['reply_to_id']?.toString(),
                replyToText: msg['reply_to_text']?.toString(),
                replyToSenderName: msg['reply_to_sender_name']?.toString(),
              );
            }).toList(),
          );
          print('✅ ${history.length} mensagens carregadas no histórico');
        });
        _scrollToBottom();

        print('📊 RESUMO DO CARREGAMENTO:');
        print('   - Total de mensagens carregadas: ${_messages.length}');
      }
    } catch (e) {
      print('❌ Erro ao carregar histórico: $e');
    } finally {
      if (mounted) {
        setState(() => _isLoadingHistory = false);
      }
    }
  }

  DateTime _parseMessageTimestamp(Map<String, dynamic> message) {
    try {
      final sentAt = message['sent_at'];
      DateTime parsedDateTime;

      if (sentAt is String && sentAt.contains('{{')) {
        parsedDateTime = _parseErlangTupleTimestamp(sentAt);
      } else if (sentAt is String) {
        parsedDateTime = DateTime.parse(sentAt);
      } else if (sentAt is int) {
        parsedDateTime = DateTime.fromMillisecondsSinceEpoch(sentAt * 1000);
      } else {
        final timestamp = message['timestamp'];
        if (timestamp != null && timestamp is int) {
          if (timestamp > 1000000000000) {
            parsedDateTime = DateTime.fromMillisecondsSinceEpoch(timestamp);
          } else {
            parsedDateTime = DateTime.fromMillisecondsSinceEpoch(
              timestamp * 1000,
            );
          }
        } else {
          throw FormatException('Nenhum formato reconhecido');
        }
      }

      // CORREÇÃO: ADICIONAR 2 HORAS
      final correctedDateTime = parsedDateTime.add(const Duration(hours: 2));
      return correctedDateTime;
    } catch (e) {
      print('❌ Erro ao parsear timestamp: $e');
    }

    return DateTime.now();
  }

  DateTime _parseErlangTupleTimestamp(String erlangTimestamp) {
    try {
      final regex = RegExp(
        r'\{\{(\d+),(\d+),(\d+)\},\{(\d+),(\d+),([\d.]+)\}\}',
      );
      final match = regex.firstMatch(erlangTimestamp);

      if (match != null) {
        final year = int.parse(match.group(1)!);
        final month = int.parse(match.group(2)!);
        final day = int.parse(match.group(3)!);
        final hour = int.parse(match.group(4)!);
        final minute = int.parse(match.group(5)!);
        final secondWithMs = double.parse(match.group(6)!);
        final second = secondWithMs.toInt();

        return DateTime(year, month, day, hour, minute, second);
      }
    } catch (e) {
      print('❌ Erro no parse Erlang: $e');
    }

    throw FormatException('Não foi possível parsear formato Erlang');
  }

  bool _isMessageFromMe(Map<String, dynamic> message) {
    final senderId = message['sender_id']?.toString();
    return senderId == _currentUserId;
  }

  Future<void> _sendMessage() async {
    final text = _messageController.text.trim();
    if (text.isEmpty || !_isConnected || _currentUserId == null) return;

    final tempMessageId =
        'temp_${DateTime.now().millisecondsSinceEpoch}_${_currentUserId}_${_uuid.v4().substring(0, 6)}';

    print('📤 Enviando mensagem: $text (ID: $tempMessageId)');

    _pendingMessageIds.add(tempMessageId);

    setState(() {
      _messages.add(
        ChatMessage(
          id: tempMessageId,
          text: text,
          isMe: true,
          timestamp: DateTime.now(),
          status: 'sent', // ícone de enviado só se realmente for ao servidor
        ),
      );

      // ✅ ORDENAR MENSAGENS POR TIMESTAMP APÓS ADICIONAR
      _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
    });

    _messageController.clear();
    _scrollToBottom();

    try {
      // Envia efetivamente (com verificação de internet)
      await ChatService.sendMessage(
        widget.remoteUserId,
        text,
        tempId: tempMessageId,
      );
    } catch (e) {
      print('❌ Falha ao enviar mensagem: $e');

      setState(() {
        _messages.removeWhere((m) => m.id == tempMessageId);
      });
      _pendingMessageIds.remove(tempMessageId);
      _messageController.text = text;

      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text('Sem conexão com a internet. Mensagem não enviada.'),
          ),
        );
      }
    }
  }

  void _scrollToBottom() {
    WidgetsBinding.instance.addPostFrameCallback((_) {
      if (_scrollController.hasClients) {
        _scrollController.animateTo(
          _scrollController.position.maxScrollExtent,
          duration: const Duration(milliseconds: 300),
          curve: Curves.easeOut,
        );
      }
    });
  }

  // SISTEMA DE DATAS
  List<MessageGroup> _groupMessagesByDate() {
    if (_messages.isEmpty) return [];

    final groups = <MessageGroup>[];
    final sortedMessages = List<ChatMessage>.from(_messages)
      ..sort((a, b) => a.timestamp.compareTo(b.timestamp));

    DateTime? currentDate;
    List<ChatMessage> currentGroup = [];

    for (final message in sortedMessages) {
      final messageDate = DateTime(
        message.timestamp.year,
        message.timestamp.month,
        message.timestamp.day,
      );

      if (currentDate == null || messageDate != currentDate) {
        if (currentGroup.isNotEmpty) {
          groups.add(
            MessageGroup(date: currentDate!, messages: List.from(currentGroup)),
          );
        }
        currentDate = messageDate;
        currentGroup = [message];
      } else {
        currentGroup.add(message);
      }
    }

    if (currentGroup.isNotEmpty) {
      groups.add(
        MessageGroup(date: currentDate!, messages: List.from(currentGroup)),
      );
    }

    return groups;
  }

  String _formatDateHeader(DateTime date) {
    final now = DateTime.now();
    final today = DateTime(now.year, now.month, now.day);
    final yesterday = DateTime(now.year, now.month, now.day - 1);
    final dateDay = DateTime(date.year, date.month, date.day);

    if (dateDay == today) {
      return 'Hoje';
    } else if (dateDay == yesterday) {
      return 'Ontem';
    } else {
      final months = [
        'Janeiro',
        'Fevereiro',
        'Março',
        'Abril',
        'Maio',
        'Junho',
        'Julho',
        'Agosto',
        'Setembro',
        'Outubro',
        'Novembro',
        'Dezembro',
      ];
      return '${date.day} de ${months[date.month - 1]}';
    }
  }

  int _calculateTotalItemCount(List<MessageGroup> groups) {
    int count = 0;
    for (final group in groups) {
      count += 1 + group.messages.length;
    }
    return count;
  }

  dynamic _getItemAtIndex(List<MessageGroup> groups, int index) {
    int currentIndex = 0;

    for (final group in groups) {
      if (index == currentIndex) {
        return group.date;
      }
      currentIndex++;

      for (final message in group.messages) {
        if (index == currentIndex) {
          return message;
        }
        currentIndex++;
      }
    }

    return null;
  }

  @override
  Widget build(BuildContext context) {
    final messageGroups = _groupMessagesByDate();

    return Scaffold(
      backgroundColor: AppTheme.backgroundColor,
      appBar: AppBar(
        backgroundColor: AppTheme.appBarColor,
        elevation: 0,
        leading: IconButton(
          icon: const Icon(Icons.arrow_back, color: AppTheme.textOnGreen),
          onPressed: () => Navigator.pop(context),
        ),
        title: Row(
          children: [
            widget.contact.photo != null
                ? CircleAvatar(
                    radius: 18,
                    backgroundImage: MemoryImage(widget.contact.photo!),
                  )
                : CircleAvatar(
                    radius: 18,
                    backgroundColor: AppTheme.surfaceColor,
                    child: Icon(
                      Icons.person,
                      color: AppTheme.avatarIcon,
                      size: 20,
                    ),
                  ),
            const SizedBox(width: 12),
            Expanded(
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                mainAxisSize: MainAxisSize.min,
                children: [
                  Text(
                    _getContactName(),
                    style: const TextStyle(
                      color: AppTheme.textOnGreen,
                      fontSize: 16,
                      fontWeight: FontWeight.w500,
                    ),
                    overflow: TextOverflow.ellipsis,
                  ),
                  if (_getPresenceText().isNotEmpty)
                    Text(
                      _getPresenceText(),
                      style: TextStyle(
                        color: _contactPresenceStatus == 'online'
                            ? AppTheme.textOnGreen.withOpacity(0.8)
                            : AppTheme.textOnGreen.withOpacity(0.6),
                        fontSize: 12,
                      ),
                      overflow: TextOverflow.ellipsis,
                    )
                  else
                    SizedBox.shrink(),
                ],
              ),
            ),
          ],
        ),
        actions: [
          IconButton(
            icon: const Icon(Icons.videocam, color: AppTheme.textOnGreen),
            onPressed: () {},
          ),
          IconButton(
            icon: const Icon(Icons.call, color: AppTheme.textOnGreen),
            onPressed: () {},
          ),
          PopupMenuButton<String>(
            icon: const Icon(Icons.more_vert, color: AppTheme.textOnGreen),
            onSelected: (value) {},
            itemBuilder: (context) => [
              const PopupMenuItem(
                value: 'info',
                child: Text('Informações do contato'),
              ),
              const PopupMenuItem(
                value: 'mute',
                child: Text('Silenciar notificações'),
              ),
            ],
          ),
        ],
      ),
      body: Column(
        children: [
          if (_isLoadingHistory)
            LinearProgressIndicator(
              backgroundColor: AppTheme.appBarColor.withOpacity(0.2),
              valueColor: const AlwaysStoppedAnimation<Color>(
                AppTheme.appBarColor,
              ),
            ),
          Expanded(
            child: _messages.isEmpty && !_isLoadingHistory
                ? _buildEmptyState()
                : ListView.builder(
                    controller: _scrollController,
                    padding: const EdgeInsets.all(16),
                    itemCount: _calculateTotalItemCount(messageGroups),
                    itemBuilder: (context, index) {
                      final item = _getItemAtIndex(messageGroups, index);

                      if (item is DateTime) {
                        return _buildDateHeader(item);
                      } else if (item is ChatMessage) {
                        return _buildMessageBubble(item);
                      } else {
                        return const SizedBox.shrink();
                      }
                    },
                  ),
          ),
          _buildMessageInput(),
        ],
      ),
    );
  }

  Widget _buildDateHeader(DateTime date) {
    return Container(
      width: double.infinity,
      padding: const EdgeInsets.symmetric(vertical: 8),
      child: Center(
        child: Container(
          padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 6),
          decoration: BoxDecoration(
            color: AppTheme.replyPreviewBackground,
            borderRadius: BorderRadius.circular(12),
          ),
          child: Text(
            _formatDateHeader(date),
            style: TextStyle(
              color: AppTheme.textSecondary,
              fontSize: 12,
              fontWeight: FontWeight.w500,
            ),
          ),
        ),
      ),
    );
  }

  Widget _buildEmptyState() {
    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Icon(Icons.chat_bubble_outline, size: 80, color: AppTheme.textLight),
          const SizedBox(height: 16),
          const Text(
            'Inicie uma conversa',
            style: TextStyle(color: AppTheme.textSecondary, fontSize: 16),
          ),
          const SizedBox(height: 8),
          Text(
            _currentUserId == null
                ? 'Carregando usuário...'
                : _isConnected
                ? 'Envie uma mensagem para começar'
                : 'Conectando ao servidor...',
            style: const TextStyle(color: AppTheme.textSecondary, fontSize: 14),
          ),
        ],
      ),
    );
  }

  // 🔔 MENU DE ANEXOS
  void _showAttachmentMenu() {
    showModalBottomSheet(
      context: context,
      backgroundColor: Colors.transparent,
      builder: (context) => Container(
        decoration: BoxDecoration(
          color: Colors.white,
          borderRadius: BorderRadius.vertical(top: Radius.circular(20)),
        ),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            // Header
            Container(
              padding: EdgeInsets.all(16),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text(
                    'Anexar',
                    style: TextStyle(fontSize: 18, fontWeight: FontWeight.w600),
                  ),
                  IconButton(
                    icon: Icon(Icons.close),
                    onPressed: () => Navigator.pop(context),
                  ),
                ],
              ),
            ),
            Divider(height: 1),

            // Opções de anexo
            Container(
              padding: EdgeInsets.all(16),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceAround,
                children: [
                  // Galeria
                  _buildAttachmentOption(
                    icon: Icons.photo_library,
                    label: 'Galeria',
                    color: AppTheme.actionEdit,
                    onTap: () {
                      Navigator.pop(context);
                      _pickFromGallery();
                    },
                  ),

                  // Câmera
                  _buildAttachmentOption(
                    icon: Icons.camera_alt,
                    label: 'Câmera',
                    color: AppTheme.appBarColor,
                    onTap: () {
                      Navigator.pop(context);
                      _pickFromCamera();
                    },
                  ),

                  // Documento
                  _buildAttachmentOption(
                    icon: Icons.insert_drive_file,
                    label: 'Documento',
                    color: Colors.orange,
                    onTap: () {
                      Navigator.pop(context);
                      _pickDocument();
                    },
                  ),

                  // Arquivo de Áudio
                  _buildAttachmentOption(
                    icon: Icons.audio_file,
                    label: 'Áudio',
                    color: Colors.red,
                    onTap: () {
                      Navigator.pop(context);
                      _pickAudioFile();
                    },
                  ),
                ],
              ),
            ),

            SizedBox(height: 20),
          ],
        ),
      ),
    );
  }

  Widget _buildAttachmentOption({
    required IconData icon,
    required String label,
    required Color color,
    required VoidCallback onTap,
  }) {
    return InkWell(
      onTap: onTap,
      borderRadius: BorderRadius.circular(12),
      child: Container(
        padding: EdgeInsets.all(16),
        decoration: BoxDecoration(
          color: color.withOpacity(0.1),
          borderRadius: BorderRadius.circular(12),
        ),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            Icon(icon, size: 32, color: color),
            SizedBox(height: 8),
            Text(
              label,
              style: TextStyle(
                fontSize: 12,
                fontWeight: FontWeight.w500,
                color: Colors.grey[700],
              ),
            ),
          ],
        ),
      ),
    );
  }

  // 📷 MÉTODOS PARA SELECIONAR ANEXOS
  Future<void> _pickFromGallery() async {
    try {
      final XFile? image = await _imagePicker.pickImage(
        source: ImageSource.gallery,
        maxWidth: 1024,
        maxHeight: 1024,
        imageQuality: 80,
      );

      if (image != null) {
        print('📷 Imagem selecionada da galeria: ${image.path}');
        // TODO: Implementar envio de imagem
        _showComingSoonSnackBar('Envio de imagens em breve!');
      }
    } catch (e) {
      print('❌ Erro ao selecionar imagem da galeria: $e');
    }
  }

  Future<void> _pickFromCamera() async {
    try {
      final XFile? image = await _imagePicker.pickImage(
        source: ImageSource.camera,
        maxWidth: 1024,
        maxHeight: 1024,
        imageQuality: 80,
      );

      if (image != null) {
        print('📷 Foto tirada com a câmera: ${image.path}');
        // TODO: Implementar envio de imagem
        _showComingSoonSnackBar('Envio de fotos em breve!');
      }
    } catch (e) {
      print('❌ Erro ao tirar foto: $e');
    }
  }

  Future<void> _pickDocument() async {
    try {
      // TODO: Implementar seleção de documentos
      print('📄 Selecionar documento');
      _showComingSoonSnackBar('Envio de documentos em breve!');
    } catch (e) {
      print('❌ Erro ao selecionar documento: $e');
    }
  }

  Future<void> _pickAudioFile() async {
    try {
      // TODO: Implementar seleção de arquivos de áudio (MP3, etc)
      print('🎵 Selecionar arquivo de áudio');
      _showComingSoonSnackBar('Envio de arquivos de áudio em breve!');
    } catch (e) {
      print('❌ Erro ao selecionar arquivo de áudio: $e');
    }
  }

  Future<void> _recordAudio() async {
    try {
      // TODO: Implementar gravação de mensagem de voz
      print('🎤 Gravar mensagem de voz');
      _showComingSoonSnackBar('Gravação de mensagem de voz em breve!');
    } catch (e) {
      print('❌ Erro ao gravar mensagem de voz: $e');
    }
  }

  // 🎤 CONTROLES DE ÁUDIO E EMOJIS
  void _toggleEmojiPicker() {
    setState(() {
      _showEmojiPicker = !_showEmojiPicker;
    });
  }

  void _insertEmoji(String emoji) {
    final text = _messageController.text;
    final cursorPosition = _messageController.selection.baseOffset;

    // Inserir emoji na posição do cursor
    final newText =
        text.substring(0, cursorPosition) +
        emoji +
        text.substring(cursorPosition);
    _messageController.value = TextEditingValue(
      text: newText,
      selection: TextSelection.collapsed(offset: cursorPosition + emoji.length),
    );

    // Fechar emoji picker após inserir
    setState(() {
      _showEmojiPicker = false;
    });
  }

  void _toggleVoiceRecording() {
    setState(() {
      _isRecording = !_isRecording;
    });

    if (_isRecording) {
      print('🎤 Iniciando gravação de mensagem de voz...');
      // TODO: Implementar gravação real
      _showComingSoonSnackBar('Gravação de mensagem de voz em breve!');
    } else {
      print('⏹️ Parando gravação de mensagem de voz');
      // TODO: Parar gravação e enviar
    }
  }

  // 📝 MÉTODOS AUXILIARES
  String _getReplyPreviewText(String messageId) {
    try {
      final message = _messages.firstWhere((msg) => msg.id == messageId);

      // Limitar texto para não quebrar layout
      String preview = message.text;
      if (preview.length > 30) {
        preview = preview.substring(0, 27) + '...';
      }

      return preview;
    } catch (e) {
      return 'mensagem não encontrada';
    }
  }

  // 📝 MÉTODOS PARA GERENCIAR MENSAGENS
  void _showMessageOptions(ChatMessage message) {
    showModalBottomSheet(
      context: context,
      backgroundColor: Colors.transparent,
      builder: (context) => Container(
        decoration: BoxDecoration(
          color: Colors.white,
          borderRadius: BorderRadius.vertical(top: Radius.circular(20)),
        ),
        child: Column(
          mainAxisSize: MainAxisSize.min,
          children: [
            // Header
            Container(
              padding: EdgeInsets.all(16),
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Text(
                    'Opções da Mensagem',
                    style: TextStyle(fontSize: 18, fontWeight: FontWeight.w600),
                  ),
                  IconButton(
                    icon: Icon(Icons.close),
                    onPressed: () => Navigator.pop(context),
                  ),
                ],
              ),
            ),
            Divider(height: 1),

            // ✅ ADICIONADO: Verificação de tempo para edição
            if (message.isMe) ...[
              if (_canEditMessage(message)) // ✅ VERIFICAÇÃO DE TEMPO
                ListTile(
                  leading: Icon(Icons.edit, color: AppTheme.actionEdit),
                  title: Text('Editar mensagem'),
                  onTap: () {
                    Navigator.pop(context);
                    _startEditingMessage(message);
                  },
                ),

              // Apagar (sempre permitido para mensagens próprias)
              ListTile(
                leading: Icon(Icons.delete, color: AppTheme.actionDelete),
                title: Text('Apagar mensagem'),
                onTap: () {
                  Navigator.pop(context);
                  _deleteMessage(message);
                },
              ),
            ],

            // Opções para todas as mensagens
            // Responder
            ListTile(
              leading: Icon(Icons.reply, color: AppTheme.appBarColor),
              title: Text('Responder'),
              onTap: () {
                Navigator.pop(context);
                _startReplyingToMessage(message);
              },
            ),

            // Copiar texto
            ListTile(
              leading: Icon(Icons.content_copy, color: AppTheme.actionCopy),
              title: Text('Copiar texto'),
              onTap: () {
                Navigator.pop(context);
                _copyMessageText(message);
              },
            ),

            // Encaminhar
            ListTile(
              leading: Icon(Icons.forward, color: Colors.orange),
              title: Text('Encaminhar'),
              onTap: () {
                Navigator.pop(context);
                _forwardMessage(message);
              },
            ),

            SizedBox(height: 20),
          ],
        ),
      ),
    );
  }

  void _startEditingMessage(ChatMessage message) {
    setState(() {
      _editingMessageId = message.id;
      _editController.text = message.text;
      _messageController.text = message.text;
    });

    // Focar no campo de edição
    FocusScope.of(context).requestFocus(FocusNode());
  }

  void _cancelEditing() {
    setState(() {
      _editingMessageId = null;
      _editController.clear();
      _messageController.clear();
    });
  }

  void _updateMessage() async {
    if (_editingMessageId != null &&
        _messageController.text.trim().isNotEmpty) {
      try {
        print(
          '✏️ Atualizando mensagem $_editingMessageId: ${_messageController.text}',
        );

        // Chamar backend para editar mensagem
        final result = await MessageOperationsService.editMessage(
          _editingMessageId!,
          _messageController.text.trim(),
        );

        if (result['success'] == true) {
          // ATUALIZAR LOCALMENTE (modal já foi fechado ao clicar em "Editar")
          // Atualizar localmente com dados do backend
          setState(() {
            final messageIndex = _messages.indexWhere(
              (msg) => msg.id == _editingMessageId,
            );
            if (messageIndex != -1) {
              final updatedMessage = result['edited_message'];
              final oldMessage = _messages[messageIndex];
              _messages[messageIndex] = ChatMessage(
                id: updatedMessage['id'].toString(),
                text: updatedMessage['content'],
                isMe: oldMessage.isMe,
                timestamp: DateTime.parse(updatedMessage['sent_at']),
                status: 'edited', // ✅ FORÇAR STATUS COMO EDITADO
                // ✅ PRESERVAR DADOS DE REPLY
                replyToId: oldMessage.replyToId,
                replyToText: oldMessage.replyToText,
                replyToSenderName: oldMessage.replyToSenderName,
                replyToSenderId: oldMessage.replyToSenderId,
              );
            }
            _editingMessageId = null;
            _editController.clear();
            _messageController.clear();
          });

          // ✅ Mensagem editada sem popup de sucesso
        }
      } catch (e) {
        print('❌ Erro ao editar mensagem: $e');
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Erro ao editar mensagem: $e'),
            backgroundColor: Colors.red,
            duration: Duration(seconds: 3),
          ),
        );
      }
    }
  }

  void _deleteMessage(ChatMessage message) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Apagar mensagem'),
        content: Column(
          mainAxisSize: MainAxisSize.min,
          crossAxisAlignment: CrossAxisAlignment.start,
          children: [
            Text('Deseja apagar esta mensagem?'),
            SizedBox(height: 8),
            Text(
              message.text.length > 50
                  ? '${message.text.substring(0, 47)}...'
                  : message.text,
              style: TextStyle(
                fontStyle: FontStyle.italic,
                color: AppTheme.textSecondary,
              ),
            ),
          ],
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Cancelar'),
          ),
          TextButton(
            onPressed: () {
              Navigator.pop(context);
              _confirmDeleteMessage(message);
            },
            child: Text('Apagar', style: TextStyle(color: Colors.red)),
          ),
        ],
      ),
    );
  }

  void _confirmDeleteMessage(ChatMessage message) async {
    try {
      print('🗑️ Apagando mensagem ${message.id}');

      // Chamar backend para deletar mensagem
      final result = await MessageOperationsService.deleteMessage(message.id);

      if (result['success'] == true) {
        // Remover mensagem localmente
        setState(() {
          _messages.removeWhere((msg) => msg.id == message.id);
        });

        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Mensagem apagada com sucesso'),
            backgroundColor: AppTheme.appBarColor,
            duration: Duration(seconds: 2),
          ),
        );
      }
    } catch (e) {
      print('❌ Erro ao apagar mensagem: $e');
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Erro ao apagar mensagem: $e'),
          backgroundColor: Colors.red,
          duration: Duration(seconds: 3),
        ),
      );
    }
  }

  void _startReplyingToMessage(ChatMessage message) {
    setState(() {
      _selectedMessageId = message.id;
    });

    // Focar no campo de mensagem
    FocusScope.of(context).requestFocus(FocusNode());
  }

  void _cancelReply() {
    setState(() {
      _selectedMessageId = null;
    });
  }

  // ======================
  // FUNÇÃO _sendReply() CORRIGIDA
  // ======================
  void _sendReply() async {
    if (_selectedMessageId == null || _selectedMessageId!.isEmpty) {
      print('❌ _selectedMessageId é nulo ou vazio');
      return;
    }

    final replyText = _messageController.text.trim();
    if (replyText.isEmpty) {
      print('❌ Texto da resposta está vazio');
      return;
    }

    // ✅ SALVAR O ID ANTES DE LIMPAR
    final originalMessageId = _selectedMessageId!;

    try {
      print('📤 ENVIANDO REPLY:');
      print('   Original ID: $originalMessageId');
      print('   Texto: $replyText');
      print('   Remote User ID: ${widget.remoteUserId}');
      print('   Current User ID: $_currentUserId');

      // ✅ 1. OBTER INFORMAÇÕES DA MENSAGEM ORIGINAL COM TRATAMENTO DE ERRO
      ChatMessage originalMessage;
      try {
        originalMessage = _messages.firstWhere(
          (msg) => msg.id == originalMessageId,
        );
        print(
          '✅ Mensagem original encontrada: ${originalMessage.text.substring(0, min(30, originalMessage.text.length))}...',
        );
      } catch (e) {
        print(
          '⚠️ Mensagem original não encontrada no histórico local, criando placeholder',
        );
        originalMessage = ChatMessage(
          id: originalMessageId,
          text: 'Mensagem não encontrada',
          isMe: false,
          timestamp: DateTime.now(),
          status: 'sent',
        );
      }

      // ✅ 2. CRIAR ID TEMPORÁRIO PARA A RESPOSTA
      final tempReplyId =
          'temp_reply_${DateTime.now().millisecondsSinceEpoch}_${_uuid.v4().substring(0, 8)}';

      print('   ID Temporário: $tempReplyId');

      // ✅ 3. CRIAR MENSAGEM LOCAL COM INFORMAÇÕES COMPLETAS
      final localReply = ChatMessage(
        id: tempReplyId,
        text: replyText,
        isMe: true,
        timestamp: DateTime.now(),
        status: 'sent',
        // ✅ INFORMAÇÕES DE REPLY PRESERVADAS
        replyToId: originalMessageId,
        replyToText: originalMessage.text,
        replyToSenderName: originalMessage.isMe ? 'Eu' : widget.contact.name,
        replyToSenderId: originalMessage.isMe
            ? _currentUserId?.toString() ?? 'unknown'
            : widget.remoteUserId,
      );

      // ✅ 4. ADICIONAR À LISTA LOCAL IMEDIATAMENTE
      setState(() {
        _messages.add(localReply);
        _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));

        // ✅ LIMPAR CAMPOS APÓS ADICIONAR LOCALMENTE
        _selectedMessageId = null;
        _messageController.clear();
      });

      // ✅ 5. SCROLL PARA BAIXO
      _scrollToBottom();

      // ✅ 6. ADICIONAR À LISTA DE PENDENTES (PARA EVITAR DUPLICAÇÃO)
      _pendingMessageIds.add(tempReplyId);

      // ✅ 7. ENVIAR PARA O BACKEND - USAR VARIÁVEL LOCAL SALVA
      print('🔄 Enviando reply para o backend...');
      final result = await MessageOperationsService.replyToMessage(
        originalMessageId,
        replyText,
        receiverId: widget.remoteUserId,
      );

      if (result['success'] == true) {
        final replyMessage = result['reply_message'];
        final dbMessageId = replyMessage['id']?.toString();

        print('✅ REPLY ENVIADO COM SUCESSO');
        print('   ID Banco: $dbMessageId');

        if (dbMessageId != null) {
          // ✅ 8. ATUALIZAR MENSAGEM LOCAL COM ID REAL DO BANCO
          final messageIndex = _messages.indexWhere(
            (msg) => msg.id == tempReplyId,
          );

          if (messageIndex != -1) {
            setState(() {
              _messages[messageIndex] = ChatMessage(
                id: dbMessageId,
                text: _messages[messageIndex].text,
                isMe: _messages[messageIndex].isMe,
                timestamp: DateTime.parse(
                  replyMessage['sent_at'] ?? DateTime.now().toIso8601String(),
                ),
                status: replyMessage['status']?.toString() ?? 'sent',
                replyToId: _messages[messageIndex].replyToId,
                replyToText: _messages[messageIndex].replyToText,
                replyToSenderName: _messages[messageIndex].replyToSenderName,
                replyToSenderId: _messages[messageIndex].replyToSenderId,
              );
            });
          }

          _pendingMessageIds.remove(tempReplyId);
        } else {
          print('⚠️ Reply enviado mas dbMessageId é nulo');
        }

        // ✅ 9. ATUALIZAR CHAT LIST
        ChatService.updateChatAfterReply(widget.remoteUserId, replyText);

        print('✅ Reply processado com sucesso!');
      } else {
        print('❌ ERRO NO BACKEND AO ENVIAR REPLY: ${result['error']}');
        // ✅ SE FALHAR, REMOVER A MENSAGEM LOCAL
        setState(() {
          _messages.removeWhere((msg) => msg.id == tempReplyId);
        });
        _pendingMessageIds.remove(tempReplyId);

        // ✅ RESTAURAR O ESTADO DE REPLY
        _selectedMessageId = originalMessageId;
        _messageController.text = replyText;

        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('Erro ao enviar resposta: ${result['error']}'),
            backgroundColor: Colors.red,
            duration: Duration(seconds: 3),
          ),
        );
      }
    } catch (e, stackTrace) {
      print('❌ ERRO CRÍTICO AO ENVIAR REPLY: $e');
      print('📚 Stack trace: $stackTrace');
      print('🔍 Estado no momento do erro:');
      print('   originalMessageId: $originalMessageId');
      print('   _selectedMessageId: $_selectedMessageId');
      print('   replyText: $replyText');
      print('   currentUserId: $_currentUserId');
      print('   remoteUserId: ${widget.remoteUserId}');

      // ✅ RESTAURAR O ESTADO PARA TENTAR NOVAMENTE
      _selectedMessageId = originalMessageId;
      _messageController.text = replyText;

      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(
          content: Text('Erro: ${e.toString()}'),
          backgroundColor: Colors.red,
          duration: Duration(seconds: 3),
        ),
      );
    }
  }

  // ✅ NOVAS FUNÇÕES AUXILIARES
  void _copyMessageText(ChatMessage message) {
    // TODO: Implementar lógica de cópia usando Clipboard
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Texto copiado para a área de transferência'),
        duration: Duration(seconds: 2),
      ),
    );
  }

  void _forwardMessage(ChatMessage message) {
    // TODO: Implementar lógica de encaminhamento
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Funcionalidade de encaminhamento em breve'),
        duration: Duration(seconds: 2),
      ),
    );
  }

  void _showComingSoonSnackBar(String message) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(message),
        backgroundColor: AppTheme.textSecondary,
        duration: Duration(seconds: 2),
      ),
    );
  }

  Widget _buildMessageInput() {
    return Column(
      children: [
        // Emoji Picker (mostra quando ativado)
        if (_showEmojiPicker)
          Container(
            height: 250,
            padding: EdgeInsets.all(8),
            decoration: BoxDecoration(
              color: AppTheme.surfaceColor,
              border: Border(top: BorderSide(color: AppTheme.dividerColor)),
            ),
            child: Column(
              children: [
                // Header do emoji picker
                Row(
                  mainAxisAlignment: MainAxisAlignment.spaceBetween,
                  children: [
                    Text(
                      'Emojis',
                      style: TextStyle(
                        fontSize: 16,
                        fontWeight: FontWeight.w600,
                      ),
                    ),
                    IconButton(
                      icon: Icon(Icons.close),
                      onPressed: _toggleEmojiPicker,
                    ),
                  ],
                ),
                Divider(height: 1),
                // Grid de emojis simples
                Expanded(
                  child: GridView.builder(
                    gridDelegate: SliverGridDelegateWithFixedCrossAxisCount(
                      crossAxisCount: 8,
                      childAspectRatio: 1.0,
                    ),
                    itemCount: _commonEmojis.length,
                    itemBuilder: (context, index) {
                      return GestureDetector(
                        onTap: () {
                          _insertEmoji(_commonEmojis[index]);
                        },
                        child: Container(
                          alignment: Alignment.center,
                          child: Text(
                            _commonEmojis[index],
                            style: TextStyle(fontSize: 24),
                          ),
                        ),
                      );
                    },
                  ),
                ),
              ],
            ),
          ),

        // Reply Preview (mostra quando respondendo)
        if (_selectedMessageId != null)
          Container(
            padding: EdgeInsets.all(12),
            decoration: BoxDecoration(
              color: AppTheme.replyPreviewBackground,
              border: Border(
                left: BorderSide(color: AppTheme.appBarColor, width: 3),
              ),
            ),
            child: Row(
              children: [
                Icon(Icons.reply, color: AppTheme.appBarColor, size: 16),
                SizedBox(width: 8),
                Expanded(
                  child: Column(
                    crossAxisAlignment: CrossAxisAlignment.start,
                    mainAxisSize: MainAxisSize.min,
                    children: [
                      Text(
                        'Respondendo a:',
                        style: TextStyle(
                          color: AppTheme.appBarColor,
                          fontSize: 12,
                          fontWeight: FontWeight.w500,
                        ),
                      ),
                      SizedBox(height: 2),
                      Text(
                        _getReplyPreviewText(_selectedMessageId!),
                        style: TextStyle(
                          color: AppTheme.replyPreviewText,
                          fontSize: 13,
                        ),
                        maxLines: 1,
                        overflow: TextOverflow.ellipsis,
                      ),
                    ],
                  ),
                ),
                IconButton(
                  icon: Icon(Icons.close, size: 16),
                  onPressed: _cancelReply,
                ),
              ],
            ),
          ),

        // Edit Preview (mostra quando editando)
        if (_editingMessageId != null)
          Container(
            padding: EdgeInsets.all(12),
            decoration: BoxDecoration(
              color: AppTheme.actionEdit.withOpacity(0.1),
              border: Border(
                left: BorderSide(color: AppTheme.actionEdit, width: 3),
              ),
            ),
            child: Row(
              children: [
                Icon(Icons.edit, color: AppTheme.actionEdit, size: 16),
                SizedBox(width: 8),
                Expanded(
                  child: Text(
                    'Editando mensagem...',
                    style: TextStyle(color: AppTheme.actionEdit),
                  ),
                ),
                IconButton(
                  icon: Icon(Icons.close, size: 16),
                  onPressed: _cancelEditing,
                ),
              ],
            ),
          ),

        // Campo de mensagem
        Container(
          padding: const EdgeInsets.all(16),
          color: AppTheme.searchBackground,
          child: Row(
            children: [
              IconButton(
                icon: Icon(Icons.attach_file, color: AppTheme.textSecondary),
                onPressed: _showAttachmentMenu,
              ),
              Expanded(
                child: Container(
                  decoration: BoxDecoration(
                    color: AppTheme.inputBackground,
                    borderRadius: BorderRadius.circular(25),
                    border: Border.all(color: AppTheme.inputBorder),
                  ),
                  child: Row(
                    children: [
                      Expanded(
                        child: TextField(
                          controller: _messageController,
                          decoration: InputDecoration(
                            hintText: _editingMessageId != null
                                ? 'Editando mensagem...'
                                : (_selectedMessageId != null
                                      ? 'Sua resposta...'
                                      : 'Digite uma mensagem...'),
                            hintStyle: TextStyle(color: AppTheme.textLight),
                            border: InputBorder.none,
                            contentPadding: EdgeInsets.symmetric(
                              horizontal: 16,
                              vertical: 12,
                            ),
                          ),
                          onChanged: (value) {
                            // Atualizar UI quando usuário digita
                            setState(() {});
                          },
                          onSubmitted: (_) {
                            if (_editingMessageId != null) {
                              _updateMessage();
                            } else if (_selectedMessageId != null) {
                              _sendReply();
                            } else {
                              _sendMessage();
                            }
                          },
                        ),
                      ),
                      IconButton(
                        icon: Icon(
                          Icons.emoji_emotions_outlined,
                          color: AppTheme.textSecondary,
                        ),
                        onPressed: _toggleEmojiPicker,
                      ),
                    ],
                  ),
                ),
              ),
              const SizedBox(width: 8),
              Container(
                decoration: BoxDecoration(
                  color: _isConnected
                      ? AppTheme.appBarColor
                      : AppTheme.textLight,
                  shape: BoxShape.circle,
                ),
                child: IconButton(
                  icon: Icon(
                    _messageController.text.trim().isEmpty
                        ? (_isRecording ? Icons.stop : Icons.mic)
                        : Icons.send,
                    color: AppTheme.textOnGreen,
                  ),
                  onPressed: _isConnected
                      ? () {
                          if (_editingMessageId != null) {
                            _updateMessage();
                          } else if (_selectedMessageId != null) {
                            _sendReply();
                          } else if (_messageController.text.trim().isEmpty) {
                            _toggleVoiceRecording();
                          } else {
                            _sendMessage();
                          }
                        }
                      : null,
                ),
              ),
            ],
          ),
        ),
      ],
    );
  }

  Widget _buildReplyPreview(ChatMessage message) {
    if (message.replyToText == null || message.replyToText!.isEmpty) {
      return const SizedBox.shrink();
    }

    // ✅ LÓGICA: Se a mensagem original foi enviada pelo usuário atual, mostrar "Eu"
    final replySenderName =
        (message.replyToSenderId == _currentUserId.toString())
        ? 'Eu'
        : message.replyToSenderName;

    return Container(
      margin: const EdgeInsets.only(bottom: 4),
      padding: const EdgeInsets.all(8),
      decoration: BoxDecoration(
        color: message.isMe
            ? AppTheme.textOnGreen.withOpacity(0.2)
            : AppTheme.replyPreviewBackground,
        borderRadius: BorderRadius.circular(8),
        border: Border(
          left: BorderSide(
            color: message.isMe
                ? AppTheme.textOnGreen.withOpacity(0.7)
                : AppTheme.appBarColor,
            width: 3,
          ),
        ),
      ),
      child: Column(
        crossAxisAlignment: CrossAxisAlignment.start,
        children: [
          if (replySenderName != null) ...[
            Text(
              replySenderName,
              style: TextStyle(
                fontSize: 11,
                fontWeight: FontWeight.bold,
                color: message.isMe
                    ? AppTheme.textOnGreen.withOpacity(0.9)
                    : AppTheme.textSecondary,
              ),
            ),
            const SizedBox(height: 2),
          ],
          Text(
            message.replyToText!,
            style: TextStyle(
              fontSize: 12,
              color: message.isMe
                  ? AppTheme.textOnGreen.withOpacity(0.9)
                  : AppTheme.textSecondary,
              fontStyle: FontStyle.italic,
            ),
            maxLines: 2,
            overflow: TextOverflow.ellipsis,
          ),
        ],
      ),
    );
  }

  Widget _buildMessageBubble(ChatMessage message) {
    return GestureDetector(
      onLongPress: () => _showMessageOptions(message),
      child: Container(
        margin: const EdgeInsets.symmetric(vertical: 4),
        child: Row(
          mainAxisAlignment: message.isMe
              ? MainAxisAlignment.end
              : MainAxisAlignment.start,
          children: [
            if (!message.isMe) ...[
              widget.contact.photo != null
                  ? CircleAvatar(
                      radius: 16,
                      backgroundImage: MemoryImage(widget.contact.photo!),
                    )
                  : CircleAvatar(
                      radius: 16,
                      backgroundColor: AppTheme.avatarBackground,
                      child: Icon(
                        Icons.person,
                        color: AppTheme.avatarIcon,
                        size: 16,
                      ),
                    ),
              const SizedBox(width: 8),
            ],
            Flexible(
              child: Container(
                padding: const EdgeInsets.symmetric(
                  horizontal: 16,
                  vertical: 10,
                ),
                decoration: BoxDecoration(
                  color: message.isMe
                      ? AppTheme.appBarColor
                      : AppTheme.messageReceived,
                  borderRadius: BorderRadius.circular(18),
                ),
                child: Column(
                  crossAxisAlignment: CrossAxisAlignment.start,
                  children: [
                    // ✅ PREVIEW DA MENSAGEM RESPONDIDA
                    _buildReplyPreview(message),

                    // ✅ TEXTO DA MENSAGEM
                    Text(
                      message.text,
                      style: TextStyle(
                        color: message.isMe
                            ? AppTheme.messageSentText
                            : AppTheme.messageReceivedText,
                        fontSize: 16,
                      ),
                    ),
                    const SizedBox(height: 4),
                    Row(
                      mainAxisSize: MainAxisSize.min,
                      children: [
                        Text(
                          _formatTime(message.timestamp),
                          style: TextStyle(
                            color: message.isMe
                                ? AppTheme.messageSentText.withOpacity(0.7)
                                : AppTheme.textLight,
                            fontSize: 10,
                          ),
                        ),
                        if (message.status == 'edited') ...[
                          SizedBox(width: 4),
                          Text(
                            'editada',
                            style: TextStyle(
                              color: message.isMe
                                  ? AppTheme.messageSentText.withOpacity(0.7)
                                  : AppTheme.textLight,
                              fontSize: 9,
                              fontStyle: FontStyle.italic,
                            ),
                          ),
                        ],
                        if (message.isMe) ...[
                          SizedBox(width: 4),
                          _buildStatusIcon(message.status),
                        ],
                      ],
                    ),
                  ],
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }

  String _formatTime(DateTime timestamp) {
    return '${timestamp.hour.toString().padLeft(2, '0')}:${timestamp.minute.toString().padLeft(2, '0')}';
  }

  Widget _buildStatusIcon(String status) {
    IconData icon;
    Color color;

    switch (status) {
      case 'read':
        icon = Icons.done_all;
        color = AppTheme.statusRead;
        break;
      case 'delivered':
      case 'received':
        icon = Icons.done_all;
        color = AppTheme.statusDelivered;
        break;
      case 'sent':
      default:
        icon = Icons.check;
        color = AppTheme.statusSent;
        break;
    }

    return Icon(icon, size: 14, color: color);
  }

  // ✅ NOVA FUNÇÃO: Verificar se mensagem pode ser editada
  bool _canEditMessage(ChatMessage message) {
    if (!message.isMe) return false;

    final now = DateTime.now();
    final difference = now.difference(message.timestamp);

    // Permitir edição por até 15 minutos
    return difference.inMinutes <= 15;
  }
}
