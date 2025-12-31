import 'package:flutter/material.dart';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:uuid/uuid.dart';
import 'package:image_picker/image_picker.dart';
import 'dart:async';
import 'dart:io';
import 'auth_service.dart';
import 'chat_service.dart';
import 'notification_service.dart';

class ChatPage extends StatefulWidget {
  final Contact contact;
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
    '🤥�',
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
    '🤥',
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
    '🤙',
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
    '🖌',
    '🖍',
    '📝',
    '✏️',
    '✒️',
    '🖊',
    '🖋',
    '🖌',
    '🖍',
    '📎',
    '📌',
    '📍',
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

  void _handleIncomingMessage(Map<String, dynamic> message) {
    print('📨 RAW MESSAGE RECEIVED: $message'); // LOG DETALHADO
    final type = message['type']?.toString();

    // ✅ TRATAMENTO ROBUSTO DE STATUS (SENT -> DELIVERED -> READ)
    if (type == 'message_delivered' || type == 'message_read') {
      final messageId = message['message_id']?.toString();
      final dbMessageId = message['db_message_id']?.toString();

      print('📥 Status Update: $type');
      print('   - ID Evento: $messageId');
      print('   - DB ID: $dbMessageId');

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
      print('📨 Mensagem para este chat: $message');
      final isFromMe = fromUserId == _currentUserId;
      final dbMessageId = message['db_message_id']?.toString();

      // ✅ CORREÇÃO CRÍTICA: Atualizar ID temporário para ID do banco
      // Relaxamos a verificação de _pendingMessageIds para garantir que o swap ocorra
      // se a mensagem existir na lista local.
      if (isFromMe && messageId != null && dbMessageId != null) {
        final idx = _messages.indexWhere((m) => m.id == messageId);
        if (idx >= 0) {
          print('🔄 SWAP DETECTADO: Confirmando envio da mensagem');
          print('   - ID Temporário: $messageId');
          print('   - ID Banco: $dbMessageId');

          setState(() {
            final old = _messages[idx];
            String statusToUse = 'sent';

            // Verifica se há status pendente para este ID (ex: race condition onde delivered chegou antes)
            if (_pendingStatusUpdates.containsKey(dbMessageId)) {
              statusToUse = _pendingStatusUpdates[dbMessageId]!;
              _pendingStatusUpdates.remove(dbMessageId);
              print('🔄 Aplicando status pendente após SWAP: $statusToUse');
            }

            _messages[idx] = ChatMessage(
              id: dbMessageId, // Atualiza para o ID oficial
              text: old.text,
              isMe: old.isMe,
              timestamp: old.timestamp,
              status: statusToUse,
            );
          });
          _pendingMessageIds.remove(messageId);
          print('   ✅ SWAP REALIZADO COM SUCESSO!');
          return; // Mensagem atualizada, interrompe o processamento
        } else {
          print(
            '⚠️ Tentativa de SWAP falhou: Mensagem $messageId não encontrada localmente.',
          );
        }
      }

      final isPendingMessage = _pendingMessageIds.contains(messageId ?? '');
      print('   - isPendingMessage: $isPendingMessage');
      print('   - messageId: $messageId');

      // ✅ VERIFICAÇÃO MELHORADA: só bloqueia se o ID já existe
      final existingMessage = _messages.any(
        (msg) =>
            (messageId != null && msg.id == messageId) ||
            (dbMessageId != null && msg.id == dbMessageId),
      );
      print('   - existingMessage: $existingMessage');
      print('   - current messages count: ${_messages.length}');

      if (!existingMessage && !isPendingMessage) {
        print('✅ ADICIONANDO MENSAGEM NOVA');
        final serverTimestamp = _parseRealTimeMessageTimestamp(message);

        // Se vier o DB ID, use-o preferencialmente
        final finalId = dbMessageId ?? messageId ?? _uuid.v4();

        setState(() {
          _messages.add(
            ChatMessage(
              id: finalId,
              text: content,
              isMe: isFromMe,
              timestamp: serverTimestamp,
              status: message['status']?.toString() ?? 'sent',
            ),
          );

          // ✅ ORDENAR MENSAGENS POR TIMESTAMP APÓS ADICIONAR
          _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
        });
        _scrollToBottom();

        if (isFromMe && messageId != null) {
          _pendingMessageIds.remove(messageId);
        } else if (!isFromMe) {
          // Mensagem recebida enquanto o chat está aberto:
          // marcar como lida APENAS se app está em foreground
          if (!_isAppInBackground) {
            print('📖 Mensagem recebida em foreground - marcando como lida');
            ChatService.markChatAsReadImmediate(widget.remoteUserId);
            ChatService.markMessagesRead(widget.remoteUserId);
          } else {
            print(
              '🌑 Mensagem recebida em background - NÃO marcando como lida',
            );

            // 🔔 ENVIAR NOTIFICAÇÃO QUANDO EM BACKGROUND
            _sendNewMessageNotification(content);
          }
        }
      } else {
        print(
          '❌ MENSAGEM BLOQUEADA: existingMessage=$existingMessage, isPendingMessage=$isPendingMessage',
        );
      }
    } else {
      print('❌ Mensagem não é para este chat');
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
    return widget.contact.displayName.isEmpty
        ? (_getContactPhone() ?? 'Sem nome')
        : widget.contact.displayName;
  }

  String? _getContactPhone() {
    return widget.contact.phones.isNotEmpty
        ? widget.contact.phones.first.number
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
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: Colors.green,
        elevation: 0,
        leading: IconButton(
          icon: const Icon(Icons.arrow_back, color: Colors.white),
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
                    backgroundColor: Colors.white,
                    child: Icon(Icons.person, color: Colors.green, size: 20),
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
                      color: Colors.white,
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
                            ? Colors.white.withOpacity(0.8)
                            : Colors.white.withOpacity(0.6),
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
            icon: const Icon(Icons.videocam, color: Colors.white),
            onPressed: () {},
          ),
          IconButton(
            icon: const Icon(Icons.call, color: Colors.white),
            onPressed: () {},
          ),
          PopupMenuButton<String>(
            icon: const Icon(Icons.more_vert, color: Colors.white),
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
              backgroundColor: Colors.green[100],
              valueColor: const AlwaysStoppedAnimation<Color>(Colors.green),
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
            color: Colors.grey[300],
            borderRadius: BorderRadius.circular(12),
          ),
          child: Text(
            _formatDateHeader(date),
            style: TextStyle(
              color: Colors.grey[700],
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
          Icon(Icons.chat_bubble_outline, size: 80, color: Colors.grey[300]),
          const SizedBox(height: 16),
          const Text(
            'Inicie uma conversa',
            style: TextStyle(color: Colors.grey, fontSize: 16),
          ),
          const SizedBox(height: 8),
          Text(
            _currentUserId == null
                ? 'Carregando usuário...'
                : _isConnected
                ? 'Envie uma mensagem para começar'
                : 'Conectando ao servidor...',
            style: const TextStyle(color: Colors.grey, fontSize: 14),
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
                    color: Colors.blue,
                    onTap: () {
                      Navigator.pop(context);
                      _pickFromGallery();
                    },
                  ),

                  // Câmera
                  _buildAttachmentOption(
                    icon: Icons.camera_alt,
                    label: 'Câmera',
                    color: Colors.green,
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

  void _showComingSoonSnackBar(String message) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(message),
        backgroundColor: Colors.grey[700],
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
              color: Colors.white,
              border: Border(top: BorderSide(color: Colors.grey[300]!)),
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

        // Campo de mensagem
        Container(
          padding: const EdgeInsets.all(16),
          color: Colors.grey[50],
          child: Row(
            children: [
              IconButton(
                icon: Icon(Icons.attach_file, color: Colors.grey[600]),
                onPressed: _showAttachmentMenu,
              ),
              Expanded(
                child: Container(
                  decoration: BoxDecoration(
                    color: Colors.white,
                    borderRadius: BorderRadius.circular(25),
                    border: Border.all(color: Colors.grey[300]!),
                  ),
                  child: Row(
                    children: [
                      Expanded(
                        child: TextField(
                          controller: _messageController,
                          decoration: const InputDecoration(
                            hintText: 'Digite uma mensagem...',
                            hintStyle: TextStyle(color: Colors.grey),
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
                          onSubmitted: (_) => _sendMessage(),
                        ),
                      ),
                      IconButton(
                        icon: Icon(
                          Icons.emoji_emotions_outlined,
                          color: Colors.grey[600],
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
                  color: _isConnected ? Colors.green : Colors.grey,
                  shape: BoxShape.circle,
                ),
                child: IconButton(
                  icon: Icon(
                    _messageController.text.trim().isEmpty
                        ? (_isRecording ? Icons.stop : Icons.mic)
                        : Icons.send,
                    color: Colors.white,
                  ),
                  onPressed: _isConnected
                      ? () {
                          if (_messageController.text.trim().isEmpty) {
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

  Widget _buildMessageBubble(ChatMessage message) {
    return Container(
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
                    backgroundColor: Colors.green[100],
                    child: Icon(Icons.person, color: Colors.green, size: 16),
                  ),
            const SizedBox(width: 8),
          ],
          Flexible(
            child: Container(
              padding: const EdgeInsets.symmetric(horizontal: 16, vertical: 10),
              decoration: BoxDecoration(
                color: message.isMe ? Colors.green : Colors.grey[200],
                borderRadius: BorderRadius.circular(18),
              ),
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                children: [
                  Text(
                    message.text,
                    style: TextStyle(
                      color: message.isMe ? Colors.white : Colors.grey[800],
                      fontSize: 16,
                    ),
                  ),
                  const SizedBox(height: 4),
                  Text(
                    _formatTime(message.timestamp),
                    style: TextStyle(
                      color: message.isMe
                          ? Colors.white.withOpacity(0.7)
                          : Colors.grey[500],
                      fontSize: 10,
                    ),
                  ),
                  if (message.isMe) ...[
                    const SizedBox(width: 4),
                    _buildStatusIcon(message.status),
                  ],
                ],
              ),
            ),
          ),
        ],
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
        color = Colors.lightBlueAccent;
        break;
      case 'delivered':
      case 'received':
        icon = Icons.done_all;
        color = Colors.white70;
        break;
      case 'sent':
      default:
        icon = Icons.check;
        color = Colors.white70;
        break;
    }

    return Icon(icon, size: 14, color: color);
  }
}

class ChatMessage {
  final String id;
  final String text;
  final bool isMe;
  final DateTime timestamp;
  final String status; // 'sent', 'delivered', 'read'

  ChatMessage({
    required this.id,
    required this.text,
    required this.isMe,
    required this.timestamp,
    required this.status,
  });
}

class MessageGroup {
  final DateTime date;
  final List<ChatMessage> messages;

  MessageGroup({required this.date, required this.messages});
}
