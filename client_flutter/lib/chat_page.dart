import 'package:flutter/material.dart';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:uuid/uuid.dart';
import 'chat_service.dart';
import 'auth_service.dart';
import 'dart:async';

class ChatPage extends StatefulWidget {
  final Contact contact;
  final String remoteUserId;

  const ChatPage({Key? key, required this.contact, required this.remoteUserId})
    : super(key: key);

  @override
  _ChatPageState createState() => _ChatPageState();
}

class _ChatPageState extends State<ChatPage> {
  final TextEditingController _messageController = TextEditingController();
  final List<ChatMessage> _messages = [];
  final ScrollController _scrollController = ScrollController();

  String? _currentUserId;
  bool _isConnected = false;
  bool _isLoadingHistory = false;
  StreamSubscription? _messageSubscription;
  StreamSubscription? _presenceSubscription;
  final Set<String> _pendingMessageIds = {};
  final Uuid _uuid = Uuid();
  bool _hasMarkedAsRead = false;
  Timer? _markAsReadTimer;

  // ✅ Status de presença do contato
  String _contactPresenceStatus = 'offline'; // 'online', 'offline'
  Timer? _presenceOnlineTimer;
  Timer? _presenceOfflineTimer;

  @override
  void initState() {
    super.initState();
    _initializeChat();

    // ✅ COMPORTAMENTO WHATSAPP: Marcar como lido ao abrir o chat
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _markAsReadOnOpen();
    });
  }

  // ✅✅✅ NOVO: Marcar como lido ao abrir (com pequeno delay)
  void _markAsReadOnOpen() {
    if (_hasMarkedAsRead) return;

    print('🚪 Chat aberto - agendando marcação como lido...');

    // ✅ Pequeno delay para garantir que tudo foi carregado
    _markAsReadTimer = Timer(Duration(milliseconds: 500), () {
      if (!_hasMarkedAsRead && mounted) {
        print('✅✅✅ WHATSAPP BEHAVIOR: Marcando chat como lido ao abrir');
        _markChatAsRead();
        _hasMarkedAsRead = true;
      }
    });
  }

  @override
  void dispose() {
    _markAsReadTimer?.cancel();
    _presenceOnlineTimer?.cancel();
    _presenceOfflineTimer?.cancel();

    // ✅ GARANTIR que marca como lido se ainda não marcou
    if (!_hasMarkedAsRead && mounted) {
      print('🚪 Saindo do chat - marcando como lido finalmente');
      _markChatAsRead();
    }

    _messageSubscription?.cancel();
    _presenceSubscription?.cancel();
    _pendingMessageIds.clear();
    super.dispose();
  }

  // ✅ Carregar status de presença do contato
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

  // ✅ Formatar status para exibição (estilo WhatsApp)
  String _getPresenceText() {
    if (_contactPresenceStatus == 'online') {
      return 'online';
    }

    // ✅ REQUISITO: quando offline, NÃO mostrar nada (campo vazio)
    // Isso significa "offline" de forma silenciosa.
    return '';
  }

  // ✅✅✅ MELHORADO: Marcar como lido com verificação
  void _markChatAsRead() {
    if (_hasMarkedAsRead) {
      print('⏳ Chat já foi marcado como lido nesta sessão');
      return;
    }

    print('📖 Marcando chat como lido (Comportamento WhatsApp)');
    ChatService.markChatAsRead(widget.remoteUserId);
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

      // ✅ ESCUTAR EVENTOS DE PRESENÇA (com delay de 2s para aparecer/sumir)
      _presenceSubscription = ChatService.presenceStream.listen((presence) {
        final userId = presence['user_id']?.toString();
        final status = presence['status']?.toString();

        if (userId == widget.remoteUserId && status != null && mounted) {
          print('📡 Evento de presença recebido: $userId -> $status');

          // Cancelar timers anteriores para evitar "piscar"
          _presenceOnlineTimer?.cancel();
          _presenceOfflineTimer?.cancel();

          if (status == 'online') {
            // ✅ Esperar 2 segundos antes de mostrar "online"
            _presenceOnlineTimer = Timer(const Duration(seconds: 2), () {
              if (!mounted) return;
              setState(() {
                _contactPresenceStatus = 'online';
              });
              print('✅ Presença aplicada (ONLINE) após delay');
            });
          } else if (status == 'offline') {
            // ✅ Esperar 2 segundos antes de remover o "online"
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

      // ✅ BUSCAR STATUS INICIAL COM DELAY DE 2s TAMBÉM
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
    final fromUserId = message['from']?.toString();
    final toUserId = message['to']?.toString();
    final messageId = message['message_id']?.toString();
    final content = message['content']?.toString() ?? '';

    final isMessageForThisChat =
        (fromUserId == widget.remoteUserId && toUserId == _currentUserId) ||
        (fromUserId == _currentUserId && toUserId == widget.remoteUserId);

    if (isMessageForThisChat && mounted) {
      final isFromMe = fromUserId == _currentUserId;

      final isPendingMessage = _pendingMessageIds.contains(messageId ?? '');
      final isDuplicate = _messages.any(
        (msg) => msg.text == content && msg.isMe == isFromMe,
      );

      if (!isDuplicate && !isPendingMessage) {
        final serverTimestamp = _parseRealTimeMessageTimestamp(message);

        setState(() {
          _messages.add(
            ChatMessage(
              id: messageId ?? _uuid.v4(),
              text: content,
              isMe: isFromMe,
              timestamp: serverTimestamp,
              status: message['status']?.toString() ?? 'sent',
            ),
          );
        });
        _scrollToBottom();

        if (isFromMe && messageId != null) {
          _pendingMessageIds.remove(messageId);
        }
      }
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

      // ✅ CORREÇÃO: ADICIONAR 2 HORAS
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

  void _sendMessage() {
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
          status: 'sent',
        ),
      );
    });

    _messageController.clear();
    _scrollToBottom();

    // ✅✅✅ CORREÇÃO: Apenas envia a mensagem - o ChatService cuida do resto
    ChatService.sendMessage(widget.remoteUserId, text, tempId: tempMessageId);

    // ❌ REMOVIDO: ChatService.updateChatContact - já é feito automaticamente no ChatService
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

  // ✅ SISTEMA DE DATAS ESTILO WHATSAPP
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

  Widget _buildMessageInput() {
    return Container(
      padding: const EdgeInsets.all(16),
      color: Colors.grey[50],
      child: Row(
        children: [
          IconButton(
            icon: Icon(Icons.attach_file, color: Colors.grey[600]),
            onPressed: () {},
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
                      onSubmitted: (_) => _sendMessage(),
                    ),
                  ),
                  IconButton(
                    icon: Icon(
                      Icons.emoji_emotions_outlined,
                      color: Colors.grey[600],
                    ),
                    onPressed: () {},
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
              icon: const Icon(Icons.send, color: Colors.white),
              onPressed: _isConnected ? _sendMessage : null,
            ),
          ),
        ],
      ),
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
