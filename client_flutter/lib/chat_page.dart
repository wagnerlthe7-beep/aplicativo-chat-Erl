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
import 'services/pending_messages_storage.dart';
import 'models/pending_message.dart';
import 'dart:math';

class ChatMessage {
  final String id;
  final String text;
  final bool isMe;
  final DateTime timestamp;
  final String status; // 'sent', 'delivered', 'read'
  final bool isEdited; // ✅ STATUS DE EDIÇÃO (sempre que is_edited for true)
  final bool isDeleted; // ✅ STATUS DE DELEÇÃO
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
    this.isEdited = false, // ✅ PADRÃO: NÃO EDITADA
    this.isDeleted = false, // ✅ PADRÃO: NÃO DELETADA
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
  List<Map<String, dynamic>> _messageHistory =
      []; // ✅ ADICIONAR HISTÓRICO LOCAL
  Timer? _markAsReadTimer;
  bool _isAppInBackground = false; // Nova variável para controlar background

  // Controles para áudio
  bool _isRecording = false;

  // Controles para edição e seleção de mensagens
  String? _selectedMessageId;
  String? _editingMessageId;
  TextEditingController _editController = TextEditingController();

  // Status de presença do contato
  String _contactPresenceStatus = 'offline'; // 'online', 'offline'
  int? _contactLastSeen; // ✅ Timestamp Unix da última vez online
  bool _isRemoteTyping = false; // ✅ Indica se o outro está digitando
  bool _isMarqueePaused = false; // ✅ Controla se o marquee está pausado
  final ScrollController _marqueeController =
      ScrollController(); // ✅ Controller para marquee
  Timer? _marqueeAnimationTimer; // ✅ Timer para animação do marquee
  GlobalKey? _marqueeTextKey; // ✅ Key para medir largura do texto
  bool _listenersConfigured = false; // ✅ Evitar múltiplos setups
  StreamSubscription? _typingSubscription;
  StreamSubscription? _connectionSubscription; // ✅ Nova subscription de conexão
  Timer? _typingTimer;
  bool _iAmTyping = false;

  Timer? _presenceOnlineTimer;
  Timer? _presenceOfflineTimer;

  @override
  void initState() {
    super.initState();
    print(
      '🔍 [INIT] ChatPage initState() - remoteUserId: ${widget.remoteUserId}',
    );
    // Registrar observer para detectar background
    WidgetsBinding.instance.addObserver(this);

    // Informar ao ChatService qual chat está ativo (para controle de unread)
    ChatService.setActiveChat(widget.remoteUserId);
    _initializeChat();
    _setupTypingListener(); // ✅ Escutar se o outro digita
    _setupMyTypingDetection(); // ✅ Detectar quando eu digito

    // Marcar como lido ao abrir o chat
    WidgetsBinding.instance.addPostFrameCallback((_) {
      _markAsReadOnOpen();
      // ✅ Cancelar todas as notificações deste chat ao abrir
      _cancelChatNotifications();
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

    ChatService.clearActiveChat(widget.remoteUserId);

    _messageSubscription?.cancel();
    _presenceSubscription?.cancel();
    _connectionSubscription?.cancel();
    _typingSubscription?.cancel();
    _typingTimer?.cancel();

    // Se eu estava digitando, avisar que parei ao sair
    if (_iAmTyping) {
      ChatService.sendTypingIndicator(widget.remoteUserId, false);
    }

    _pendingMessageIds.clear();

    // ✅ Limpar controller e timer do marquee
    _marqueeAnimationTimer?.cancel();
    _marqueeController.dispose();
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

  // ✅ Escutar se o outro está digitando
  void _setupTypingListener() {
    _typingSubscription = ChatService.typingStream.listen((data) {
      if (data['from'] == widget.remoteUserId && mounted) {
        setState(() {
          _isRemoteTyping = data['is_typing'] ?? false;
        });
      }
    });
  }

  // ✅ Detectar quando eu estou digitando
  void _setupMyTypingDetection() {
    _messageController.addListener(() {
      final text = _messageController.text;

      if (text.isNotEmpty) {
        if (!_iAmTyping) {
          _iAmTyping = true;
          ChatService.sendTypingIndicator(widget.remoteUserId, true);

          // Iniciar pulso/heartbeart para manter o status ativo no outro lado
          // (evita que o timer de segurança do outro lado expire)
          _typingHeartbeatTimer?.cancel();
          _typingHeartbeatTimer = Timer.periodic(Duration(seconds: 4), (timer) {
            if (_iAmTyping && mounted) {
              ChatService.sendTypingIndicator(widget.remoteUserId, true);
            } else {
              timer.cancel();
            }
          });
        }

        // Reiniciar o timer de "parou de digitar" (2 segundos de silêncio)
        _typingTimer?.cancel();
        _typingTimer = Timer(Duration(seconds: 2), () {
          if (_iAmTyping && mounted) {
            _stopTyping();
          }
        });
      } else {
        // Se apagou tudo, parar imediatamente
        if (_iAmTyping) {
          _stopTyping();
        }
      }
    });
  }

  void _stopTyping() {
    _iAmTyping = false;
    _typingHeartbeatTimer?.cancel();
    _typingTimer?.cancel();
    ChatService.sendTypingIndicator(widget.remoteUserId, false);
  }

  Timer? _typingHeartbeatTimer;

  // ✅ REMOVIDO: HTTP call ao abrir chat
  // Presença é atualizada APENAS via eventos WebSocket
  // Se não houver evento recente, mostrar nada (não inferir estado)
  Future<void> _loadContactPresence() async {
    // ✅ Usar apenas estado em cache (recebido via eventos WebSocket)
    // NÃO fazer HTTP call - isso causa inconsistência porque o servidor
    // pode retornar "online" baseado no socket mesmo quando está em background
    final cachedStatus = ChatService.getCachedPresenceStatus(
      widget.remoteUserId,
    );

    if (cachedStatus != null && mounted) {
      print('📦 Usando presença em cache: $cachedStatus');
      setState(() {
        _contactPresenceStatus = cachedStatus;
        // ✅ Se estiver em background, NUNCA mostrar last_seen
        if (cachedStatus == 'background') {
          _contactLastSeen = null;
        } else if (cachedStatus == 'offline') {
          // ✅ Para offline, podemos mostrar last_seen se disponível
          // Mas apenas se recebido via evento (não via HTTP)
          _contactLastSeen = null; // Será atualizado via eventos
        } else {
          // ✅ Para online, não mostrar "Online há..." - apenas "Online"
          _contactLastSeen = null;
        }
        _isMarqueePaused = false;
        _resetMarquee();
      });
    } else {
      // ✅ Se não houver estado em cache, mostrar nada (não inferir)
      // Aguardar próximo evento de presença via WebSocket
      if (mounted) {
        setState(() {
          _contactPresenceStatus =
              'offline'; // Estado padrão até receber evento
          _contactLastSeen = null;
        });
      }
      print('⏳ Aguardando evento de presença via WebSocket...');
    }
  }

  // Formatar status para exibição
  // ✅ REGRA: Presença é evento, não inferência
  // Só mostrar o que foi recebido via eventos WebSocket
  String _getPresenceText() {
    if (_isRemoteTyping) return 'a escrever...'; // ✅ Prioridade máxima

    // ✅ Online: mostrar apenas "online" (nunca "Online há X")
    if (_contactPresenceStatus == 'online') {
      return 'online';
    }

    // ✅ Background: app minimizada - NÃO mostrar nada
    if (_contactPresenceStatus == 'background') {
      return '';
    }

    // ✅ Offline: mostrar last_seen APENAS se recebido via evento
    // NUNCA calcular "Online há X" - isso é inferência local (ERRADO)
    // WhatsApp mostra apenas "última vez online: [data/hora]" quando realmente offline
    if (_contactPresenceStatus == 'offline' && _contactLastSeen != null) {
      return _formatLastSeen(_contactLastSeen!);
    }

    // ✅ Se não há estado claro, não mostrar nada
    return '';
  }

  // ✅ Formatar last_seen de forma amigável
  // ✅ IMPORTANTE: Só mostrar quando status é realmente "offline"
  // NUNCA mostrar "Online há X" - isso não existe no WhatsApp
  String _formatLastSeen(int timestamp) {
    final lastSeenDate = DateTime.fromMillisecondsSinceEpoch(timestamp * 1000);
    final now = DateTime.now();
    final difference = now.difference(lastSeenDate);

    // ✅ WhatsApp-style: mostrar apenas data/hora, nunca "Online há X"
    if (difference.inDays == 0) {
      // Hoje - mostrar hora
      return 'última vez online: ${lastSeenDate.hour.toString().padLeft(2, '0')}:${lastSeenDate.minute.toString().padLeft(2, '0')}';
    } else if (difference.inDays == 1) {
      return 'última vez online: ontem às ${lastSeenDate.hour.toString().padLeft(2, '0')}:${lastSeenDate.minute.toString().padLeft(2, '0')}';
    } else if (difference.inDays < 7) {
      // Esta semana - mostrar dia da semana
      final weekdays = ['Dom', 'Seg', 'Ter', 'Qua', 'Qui', 'Sex', 'Sáb'];
      return 'última vez online: ${weekdays[lastSeenDate.weekday % 7]} às ${lastSeenDate.hour.toString().padLeft(2, '0')}:${lastSeenDate.minute.toString().padLeft(2, '0')}';
    } else {
      // Mais de uma semana - mostrar data completa
      final day = lastSeenDate.day.toString().padLeft(2, '0');
      final month = lastSeenDate.month.toString().padLeft(2, '0');
      final year = lastSeenDate.year;
      return 'última vez online: $day/$month/$year';
    }
  }

  // ✅ Widget de marquee para texto que rola da direita para esquerda
  Widget _buildMarqueeText() {
    final text = _getPresenceText();
    final isOnline = _contactPresenceStatus == 'online';

    // ✅ Se é "online", não precisa marquee
    if (isOnline) {
      return Text(
        text,
        style: TextStyle(
          color: AppTheme.textOnGreen.withOpacity(0.8),
          fontSize: 12,
        ),
      );
    }

    // ✅ Inicializar key se necessário
    _marqueeTextKey ??= GlobalKey();

    // ✅ Iniciar animação se não estiver pausada
    if (!_isMarqueePaused) {
      WidgetsBinding.instance.addPostFrameCallback((_) {
        _startMarqueeAnimation(text);
      });
    }

    // ✅ Para "last_seen", usar marquee (como WhatsApp)
    return GestureDetector(
      onTap: () {
        // ✅ Pausar quando usuário tocar
        setState(() {
          _isMarqueePaused = true;
          _marqueeAnimationTimer?.cancel();
        });
      },
      child: SizedBox(
        height: 16,
        child: ClipRect(
          clipBehavior: Clip.hardEdge, // ✅ Garantir que o clip seja rígido
          child: SingleChildScrollView(
            controller: _marqueeController,
            scrollDirection: Axis.horizontal,
            physics: _isMarqueePaused
                ? NeverScrollableScrollPhysics()
                : ClampingScrollPhysics(),
            child: Text(
              text,
              key: _marqueeTextKey,
              style: TextStyle(
                color: AppTheme.textOnGreen.withOpacity(0.6),
                fontSize: 12,
              ),
            ),
          ),
        ),
      ),
    );
  }

  // ✅ Resetar marquee (com verificação de controller anexado)
  void _resetMarquee() {
    if (!mounted) return;
    // ✅ Verificar se controller está anexado antes de usar
    if (_marqueeController.hasClients) {
      _marqueeController.jumpTo(0);
    } else {
      // ✅ Se não está anexado, aguardar e tentar novamente
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (mounted && _marqueeController.hasClients) {
          _marqueeController.jumpTo(0);
        }
      });
    }
  }

  // ✅ Iniciar animação do marquee
  void _startMarqueeAnimation(String text) {
    if (_isMarqueePaused || !mounted) return;

    // ✅ Cancelar timer anterior
    _marqueeAnimationTimer?.cancel();

    // ✅ Aguardar para garantir que o widget está renderizado
    Future.delayed(const Duration(milliseconds: 500), () {
      if (!mounted || _isMarqueePaused) return;

      // ✅ Medir largura do texto
      final RenderBox? renderBox =
          _marqueeTextKey?.currentContext?.findRenderObject() as RenderBox?;
      if (renderBox == null) {
        // ✅ Tentar novamente após um delay
        Future.delayed(const Duration(milliseconds: 300), () {
          if (mounted && !_isMarqueePaused) {
            _startMarqueeAnimation(text);
          }
        });
        return;
      }

      final textWidth = renderBox.size.width;

      // ✅ Obter largura do container do contexto do ScrollView
      final ScrollController? controller = _marqueeController;
      if (controller == null || !controller.hasClients) {
        Future.delayed(const Duration(milliseconds: 200), () {
          if (mounted && !_isMarqueePaused) {
            _startMarqueeAnimation(text);
          }
        });
        return;
      }

      // ✅ Obter largura do container através do ScrollPosition
      final containerWidth = controller.position.viewportDimension;

      // ✅ Se o texto cabe no container, não precisa rolar
      if (textWidth <= containerWidth) {
        return;
      }

      // ✅ Verificar se controller está anexado antes de animar
      if (!_marqueeController.hasClients) {
        // ✅ Tentar novamente após um delay
        Future.delayed(const Duration(milliseconds: 300), () {
          if (mounted && !_isMarqueePaused) {
            _startMarqueeAnimation(text);
          }
        });
        return;
      }

      // ✅ Detectar qual prefixo está sendo usado no texto
      String prefix = 'Online há ';
      if (text.startsWith('Online ontem às ')) {
        prefix = 'Online ontem às ';
      } else if (text.startsWith('última vez online: ')) {
        prefix = 'última vez online: ';
      } else if (text.startsWith('Online há ')) {
        prefix = 'Online há ';
      }

      // ✅ Criar TextPainter para o prefixo com o mesmo estilo exato usado no widget
      final prefixPainter = TextPainter(
        text: TextSpan(
          text: prefix,
          style: TextStyle(
            color: AppTheme.textOnGreen.withOpacity(0.6),
            fontSize: 12,
          ),
        ),
        textDirection: TextDirection.ltr,
        maxLines: 1,
      );
      prefixPainter.layout();
      final prefixWidth = prefixPainter.size.width;

      // ✅ Debug: imprimir valores para verificar
      print(
        '🔍 Marquee Debug: prefixWidth=$prefixWidth, textWidth=$textWidth, containerWidth=$containerWidth',
      );

      // ✅ Calcular distância para rolar até mostrar apenas a parte do tempo
      // Usar um offset muito maior para garantir que o prefixo fique completamente fora
      // Multiplicar por 1.2 para garantir margem extra (20% a mais)
      final scrollDistance =
          (prefixWidth * 1.2) + 60; // +60 pixels de margem extra

      print('🔍 Marquee Debug: scrollDistance=$scrollDistance');

      // ✅ Aguardar 1.5 segundos antes de começar a rolar (como WhatsApp)
      Future.delayed(const Duration(milliseconds: 1500), () {
        if (!mounted || _isMarqueePaused || !_marqueeController.hasClients)
          return;

        // ✅ Rolar da direita (0) para esquerda (scrollDistance)
        // Isso faz o texto rolar até esconder "última vez online: " e mostrar apenas "há X min"
        _marqueeController
            .animateTo(
              scrollDistance,
              duration: Duration(
                milliseconds: (scrollDistance * 30).toInt().clamp(1000, 5000),
              ), // Velocidade: 30ms por pixel (min 1s, max 5s) - mais lento para ler
              curve: Curves.linear,
            )
            .then((_) {
              // ✅ Quando terminar de rolar, manter na posição final (pausado)
              // Apenas a parte do tempo (ex: "há 1 min") estará visível
              if (mounted && !_isMarqueePaused) {
                setState(() {
                  _isMarqueePaused =
                      true; // ✅ Parar quando mostrar apenas o tempo
                });
              }
            });
      });
    });
  }

  // MELHORADO: Marcar como lido com verificação
  /// Cancelar todas as notificações deste chat
  void _cancelChatNotifications() async {
    try {
      await NotificationService().cancelChatNotifications(widget.remoteUserId);
      print('✅ [ChatPage] Notificações do chat ${widget.remoteUserId} canceladas');
    } catch (e) {
      print('❌ [ChatPage] Erro ao cancelar notificações: $e');
    }
  }

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

      // ✅ MUDANÇA CRÍTICA: Carregar histórico PRIMEIRO (offline-first real)
      // Não aguardar o setupRealChat que pode demorar 5s se o server estiver down
      _loadChatHistory();

      // Conectar em background (sem await para não bloquear a UI)
      _setupRealChat();
    } catch (e) {
      print('❌ Erro na inicialização do chat: $e');
    }
  }

  Future<void> _setupRealChat() async {
    print('🔍 [SETUP] _setupRealChat() chamado');
    final connected = await ChatService.connect();
    print('🔍 [SETUP] Conectado: $connected');

    if (mounted) {
      setState(() {
        _isConnected = connected;
      });
    }

    if (connected) {
      print('🔍 [SETUP] Entrou no if (connected) - configurando listeners');
      // ✅ ONLINE: Configurar listeners em tempo real
      _messageSubscription = ChatService.messageStream.listen((message) {
        print('💬 Mensagem recebida: $message');
        _handleIncomingMessage(message);
      });

      // ESCUTAR EVENTOS DE PRESENÇA (com delay de 2s para aparecer/sumir)
      print(
        '🔍 [SETUP] Configurando presenceSubscription para remoteUserId: ${widget.remoteUserId}',
      );
      _presenceSubscription = ChatService.presenceStream.listen((presence) {
        final userId = presence['user_id']?.toString();
        final status = presence['status']?.toString();

        print(
          '🔍 [PRESENCE DEBUG] Evento recebido: userId=$userId, status=$status, target=${widget.remoteUserId}',
        );

        if (userId == widget.remoteUserId && status != null && mounted) {
          print('📡 Evento de presença recebido: $userId -> $status');

          // Cancelar timers anteriores para evitar "piscar"
          _presenceOnlineTimer?.cancel();
          _presenceOfflineTimer?.cancel();

          if (status == 'online') {
            // ATUALIZAR IMEDIATAMENTE
            print('⚡ Atualizando IMEDIATAMENTE para ONLINE: $userId');
            if (mounted) {
              setState(() {
                _contactPresenceStatus = 'online';
                _isMarqueePaused = false;
                _resetMarquee();
              });
              print('✅ Presença aplicada (ONLINE) imediatamente');
            }
          } else if (status == 'background') {
            // App em background - não mostrar nada
            print('🌑 Atualizando para BACKGROUND: $userId');
            if (mounted) {
              setState(() {
                _contactPresenceStatus = 'background';
                _contactLastSeen =
                    null; // ✅ Limpar last_seen quando em background (não mostrar "Online há...")
                _isMarqueePaused = false;
                _resetMarquee();
              });
              print(
                '✅ Presença aplicada (BACKGROUND) - UI vazia, last_seen limpo',
              );
            }
          } else if (status == 'offline') {
            // ATUALIZAR IMEDIATAMENTE
            print('⚡ Atualizando IMEDIATAMENTE para OFFLINE: $userId');
            if (mounted) {
              setState(() {
                _contactPresenceStatus = 'offline';
                // ✅ Usar last_seen do evento se disponível, senão null
                final lastSeen = presence['last_seen'];
                if (lastSeen != null) {
                  _contactLastSeen = lastSeen is int
                      ? lastSeen
                      : int.tryParse(lastSeen.toString());
                } else {
                  _contactLastSeen =
                      null; // ✅ Se não vier no evento, não mostrar nada
                }
                _isMarqueePaused = false;
                _resetMarquee();
              });
              print(
                '✅ Presença aplicada (OFFLINE) - last_seen: $_contactLastSeen',
              );
            }
            // ✅ REMOVIDO: HTTP call ao receber evento offline
            // O last_seen deve vir no próprio evento de presença
            // Se não vier no evento, não mostrar nada (não inferir, não buscar via HTTP)
          }
        }
      });

      // ✅ ESCUTAR EVENTOS DE RECONEXÃO
      _connectionSubscription = ChatService.connectionStatusStream.listen((
        isConnected,
      ) {
        if (isConnected && mounted) {
          print('🔄 WebSocket reconectado - aguardando eventos de presença...');
          // ✅ REMOVIDO: refreshUserPresence() faz HTTP call
          // Aguardar eventos WebSocket em vez de fazer HTTP call
          // O servidor enviará eventos de presença automaticamente após reconexão
        }
      });

      // ✅ REMOVIDO: HTTP call com delay de 2s
      // Usar apenas estado em cache (recebido via eventos WebSocket)
      // Se não houver cache, mostrar nada até receber evento

      // ✅ Carregar presença do cache ao abrir o chat
      // Isso garante que se o usuário está online e continua online,
      // o status será mostrado mesmo sem novo evento
      _loadContactPresence();

      // ✅ NOVO: Listener para atualizar status em tempo real
      _startStatusUpdateListener();
    } else {
      // ✅ OFFLINE: Modo offline - sem presença, sem listeners em tempo real
      print('⚠️ Modo offline - histórico local será carregado');
      // ✅ Tentar carregar presença do cache mesmo em modo offline
      // (pode ter cache de quando estava online)
      // Se não houver cache, _loadContactPresence() já define como 'offline'
      _loadContactPresence();

      // NOVO: Configurar listener de conexão para quando voltar
      _connectionSubscription = ChatService.connectionStatusStream.listen((
        isConnected,
      ) {
        print(' [CONNECTION] Status mudou: $isConnected');
        if (isConnected && mounted && !_listenersConfigured) {
          print(' [CONNECTION] Conectou! Configurando listeners...');
          _listenersConfigured = true; // ✅ Marcar como configurado

          // ✅ CANCELAR LISTENERS ANTIGOS para evitar duplicação
          _presenceSubscription?.cancel();
          _messageSubscription?.cancel();

          _presenceSubscription = ChatService.presenceStream.listen((presence) {
            final userId = presence['user_id']?.toString();
            final status = presence['status']?.toString();

            print(
              ' [PRESENCE DEBUG] Evento recebido: userId=$userId, status=$status, target=${widget.remoteUserId}',
            );

            if (userId == widget.remoteUserId && status != null && mounted) {
              print(' Evento de presença recebido: $userId -> $status');

              // Cancelar timers anteriores para evitar "piscar"
              _presenceOnlineTimer?.cancel();
              _presenceOfflineTimer?.cancel();

              if (status == 'online') {
                print('⚡ Atualizando para ONLINE: $userId');
                if (mounted) {
                  setState(() {
                    _contactPresenceStatus = 'online';
                  });
                }
              } else if (status == 'background') {
                print('🌑 Atualizando para BACKGROUND: $userId');
                if (mounted) {
                  setState(() {
                    _contactPresenceStatus = 'background';
                    _contactLastSeen =
                        null; // ✅ Limpar last_seen quando em background
                  });
                }
              } else if (status == 'offline') {
                print('⚡ Atualizando para OFFLINE: $userId');
                if (mounted) {
                  setState(() {
                    _contactPresenceStatus = 'offline';
                  });
                  print('✅ Presença aplicada (OFFLINE)');
                }
              }
            }
          });

          _messageSubscription = ChatService.messageStream.listen((message) {
            print(' Mensagem recebida: $message');
            _handleIncomingMessage(message);
          });
        }
      });
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
  void _handleIncomingMessage(Map<String, dynamic> message) async {
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
      final isEditedFromBackend = message['is_edited'] ?? false;

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
              isEdited:
                  oldMsg.isEdited ||
                  isEditedFromBackend, // ✅ COMBINAR STATUS DE EDIÇÃO!
              isDeleted: oldMsg.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
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

          final candidates = _messages
              .where(
                (m) =>
                    m.isMe &&
                    (m.status == 'sent' || m.status == 'pending_local') &&
                    int.tryParse(m.id) == null &&
                    m.replyToId == null,
              )
              .toList();

          final candidateIdx = candidates.length == 1
              ? _messages.indexOf(candidates.first)
              : -1;

          if (candidateIdx >= 0) {
            final oldMsg = _messages[candidateIdx];
            final isRecent =
                DateTime.now().difference(oldMsg.timestamp).inSeconds < 30;
            if (!isRecent) {
              print(
                '⚠️ Heurística ignorada: mensagem candidata antiga demais (${oldMsg.id})',
              );
              _pendingStatusUpdates[dbMessageId] = newStatus;
              print('   📌 Status "$newStatus" guardado para ID $dbMessageId');
              return;
            }
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
                isEdited:
                    oldMsg.isEdited ||
                    isEditedFromBackend, // ✅ COMBINAR STATUS DE EDIÇÃO!
                isDeleted: oldMsg.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
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

    final isFromMe = fromUserId == _currentUserId;
    final dbMessageId = message['db_message_id']?.toString();

    final isMessageForThisChat =
        (fromUserId == widget.remoteUserId && toUserId == _currentUserId) ||
        (fromUserId == _currentUserId && toUserId == widget.remoteUserId);

    if (isMessageForThisChat && mounted) {
      print('📨 Mensagem recebida: $message');
      print(
        '🔍 DEBUG: isFromMe=$isFromMe, fromUserId=$fromUserId, toUserId=$toUserId',
      );
      print(
        '🔍 DEBUG: _currentUserId=$_currentUserId, widget.remoteUserId=${widget.remoteUserId}',
      );

      // ✅ DETECTAR SE É UMA RESPOSTA
      final isReply = message['reply_to_id'] != null;
      if (isReply) {
        print('🔍 MENSAGEM É UMA RESPOSTA!');
        print('   reply_to_id: ${message['reply_to_id']}');
        print('   reply_to_text: ${message['reply_to_text']}');
        print('   reply_to_sender_name: ${message['reply_to_sender_name']}');
      }

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

            // ✅ Usar nome do contato (que aparece no topo), não o nome do BD
            final replyToSenderName =
                old.replyToSenderName ?? widget.contact.name;

            _messages[idx] = ChatMessage(
              id: dbMessageId,
              text: old.text,
              isMe: old.isMe,
              timestamp: _parseRealTimeMessageTimestamp(
                message,
              ), // ✅ Usar timestamp do servidor
              status: finalStatus,
              isEdited: old.isEdited, // ✅ PRESERVAR STATUS DE EDIÇÃO!
              isDeleted: old.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
              // ✅ PRESERVAR INFORMAÇÕES DE REPLY (usar nome do contato)
              replyToId: old.replyToId,
              replyToText: old.replyToText,
              replyToSenderName: replyToSenderName, // ✅ Usar nome do contato
              replyToSenderId: old.replyToSenderId,
            );

            // ✅ NOVO: Atualizar status no histórico local para consistência offline
            if (_currentUserId != null) {
              ChatService.updateMessageStatusInHistory(
                _currentUserId!,
                widget.remoteUserId,
                messageId,
                finalStatus,
                dbMessageId: dbMessageId,
              );
            }
          });
          _pendingMessageIds.remove(messageId);
          return;
        }
      }

      // ✅ SWAP HEURÍSTICO PARA REPLIES (quando o servidor envia só o ID real)
      // ✅ CORRIGIDO: Procurar também por pending_local e sent
      if (isFromMe &&
          dbMessageId != null &&
          (message['reply_to_id'] != null ||
              message['reply_to_text'] != null)) {
        final pendingIdx = _messages.indexWhere(
          (m) =>
              m.isMe &&
              (m.status == 'sent' ||
                  m.status == 'pending_local') && // ✅ Incluir pending_local
              m.replyToId == message['reply_to_id']?.toString() &&
              m.text == content,
        );

        if (pendingIdx >= 0) {
          final old = _messages[pendingIdx];
          print(
            '🔄 SWAP HEURÍSTICO DE REPLY: ${old.id} -> $dbMessageId (status: ${old.status} -> ${message['status']})',
          );
          setState(() {
            // ❗ Não fazer downgrade de status: se já está delivered/read, não voltar para 'sent'
            final incomingStatus = message['status']?.toString() ?? 'sent';
            final finalStatus =
                (old.status == 'read' || old.status == 'delivered') &&
                    incomingStatus == 'sent'
                ? old.status
                : incomingStatus;

            print(
              '   Status final: $finalStatus (incoming: $incomingStatus, old: ${old.status})',
            );

            // ✅ Usar nome do contato (que aparece no topo), não o nome do BD
            final replyToSenderName =
                old.replyToSenderName ?? widget.contact.name;

            _messages[pendingIdx] = ChatMessage(
              id: dbMessageId,
              text: old.text,
              isMe: old.isMe,
              timestamp: _parseRealTimeMessageTimestamp(
                message,
              ), // ✅ Usar timestamp do servidor
              status: finalStatus,
              isEdited: old.isEdited, // ✅ PRESERVAR STATUS DE EDIÇÃO!
              isDeleted: old.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
              replyToId: old.replyToId,
              replyToText: old.replyToText,
              replyToSenderName: replyToSenderName, // ✅ Usar nome do contato
              replyToSenderId: old.replyToSenderId,
            );

            // ✅ NOVO: Atualizar status no histórico local para consistência offline
            if (_currentUserId != null) {
              ChatService.updateMessageStatusInHistory(
                _currentUserId!,
                widget.remoteUserId,
                old.id,
                finalStatus,
                dbMessageId: dbMessageId,
              );
            }
          });
          _pendingMessageIds.remove(old.id);
          return;
        }
      }

      // ✅ VERIFICAÇÃO DE DUPLICAÇÃO MELHORADA (incluindo replies)
      final isPendingMessage = _pendingMessageIds.contains(messageId ?? '');

      // ✅ Verificar se mensagem já existe por ID ou por conteúdo + reply (para replies)
      final existingMessage = _messages.any((msg) {
        // Verificar por ID
        if ((messageId != null && msg.id == messageId) ||
            (dbMessageId != null && msg.id == dbMessageId)) {
          return true;
        }

        // ✅ Para replies, verificar também por conteúdo + reply_to_id (evitar duplicação)
        if (isFromMe && message['reply_to_id'] != null) {
          final replyToId = message['reply_to_id']?.toString();
          if (msg.isMe &&
              msg.text == content &&
              msg.replyToId == replyToId &&
              (msg.status == 'sent' ||
                  msg.status == 'delivered' ||
                  msg.status == 'read')) {
            // ✅ Se já existe uma mensagem com mesmo conteúdo e reply_to_id e status não pendente, é duplicada
            print(
              '⚠️ Reply duplicada detectada e ignorada: $content (já existe com status ${msg.status})',
            );
            return true;
          }
        }

        return false;
      });

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
              isDeleted: false, // ✅ PADRÃO: NÃO DELETADA
              // ✅ INFORMAÇÕES DE REPLY (SE HOUVER)
              // ✅ Usar nome do contato (que aparece no topo), não o nome do BD
              replyToId: message['reply_to_id']?.toString(),
              replyToText: message['reply_to_text']?.toString(),
              replyToSenderName:
                  widget.contact.name, // ✅ Sempre usar nome do contato
              replyToSenderId: message['reply_to_sender_id']?.toString(),
            ),
          );

          _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
        });

        _scrollToBottom();

        // ✅ NOVO: Salvar mensagem recebida no histórico local para persistência offline
        if (!isFromMe) {
          print('💾 Salvando mensagem recebida no histórico local: $finalId');
          await ChatService.saveMessageToLocalHistory(
            _currentUserId!,
            widget.remoteUserId,
            {
              'message_id': finalId,
              'content': content,
              'sender_id': message['sender_id'],
              'receiver_id': _currentUserId,
              'sent_at': serverTimestamp.toIso8601String(),
              'status': message['status']?.toString() ?? 'sent',
              'is_edited': message['is_edited'] ?? false,
              'is_deleted': false,
              // ✅ Campos de reply
              'reply_to_id': message['reply_to_id']?.toString(),
              'reply_to_text': message['reply_to_text']?.toString(),
              'reply_to_sender_name':
                  widget.contact.name, // ✅ Usar nome do contato
              'reply_to_sender_id': message['reply_to_sender_id']?.toString(),
            },
          );
          print('✅ Mensagem recebida salva no histórico local');
        }

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
            isDeleted: false, // ✅ PADRÃO: NÃO DELETADA
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
        isDeleted: false, // ✅ PADRÃO: NÃO DELETADA
        isEdited: false, // Será true quando for editada
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

  // ✅ FUNÇÃO PARA PERSONALIZAR TEXTO DE MENSAGENS DELETADAS
  String _getDeletedMessageText(Map<String, dynamic> msg) {
    if (msg['is_deleted'] == true) {
      final deletedBy = msg['deleted_by']?.toString();

      // ✅ PERSONALIZAR BASEADO EM QUEM DELETOU
      if (deletedBy == _currentUserId?.toString()) {
        // EU apaguei a mensagem
        return '⊗ Eliminou esta mensagem';
      } else {
        // OUTRA pessoa apagou a mensagem
        return '⊗ Esta mensagem foi apagada';
      }
    }

    // ✅ SE NÃO ESTIVER DELETADA, USAR CONTEÚDO NORMAL
    return msg['content'] ?? '';
  }

  // Handler para mensagens deletadas recebidas via WebSocket
  void _handleDeletedMessage(Map<String, dynamic> message) {
    final messageId = message['message_id']?.toString();
    final deletedBy = message['deleted_by']?.toString();

    if (messageId != null) {
      // ✅ Ignorar deleções "fantasma" que dizem que EU deletei
      // sem ter iniciado a deleção localmente.
      if (deletedBy == _currentUserId?.toString() &&
          !_localDeleteRequests.contains(messageId)) {
        print('⚠️ Ignorando deleção não solicitada localmente: $messageId');
        return;
      }
      setState(() {
        final messageIndex = _messages.indexWhere((msg) => msg.id == messageId);
        if (messageIndex != -1) {
          final oldMessage = _messages[messageIndex];

          // ✅ PERSONALIZAR MENSAGEM BASEADO EM QUEM DELETOU (não em quem enviou)
          String deleteText;
          if (deletedBy == _currentUserId?.toString()) {
            // EU apaguei a mensagem
            deleteText = '⊗ Eliminou esta mensagem';
          } else {
            // OUTRA pessoa apagou a mensagem
            deleteText = '⊗ Esta mensagem foi apagada';
          }

          _messages[messageIndex] = ChatMessage(
            id: oldMessage.id,
            text: deleteText,
            isMe: oldMessage.isMe,
            timestamp: oldMessage.timestamp,
            status: oldMessage.status,
            isEdited: false, // ✅ NÃO MOSTRAR STATUS EDIT
            isDeleted: true, // ✅ MARCAR COMO DELETADA
            replyToId: oldMessage.replyToId,
            replyToText: oldMessage.replyToText,
            replyToSenderName: oldMessage.replyToSenderName,
            replyToSenderId: oldMessage.replyToSenderId,
          );
          print(
            '✅ Mensagem ${messageId} marcada como deletada: $deleteText (deleted_by: $deletedBy)',
          );
        }
      });
      _localDeleteRequests.remove(messageId);
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
            status: oldMessage
                .status, // ✅ PRESERVAR STATUS DELIVERY (sent/delivered/read)
            isEdited: true, // ✅ MARCAR COMO EDITADA
            isDeleted: oldMessage.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
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
          // ✅ Timestamp em milissegundos - usar mesma lógica do chat_list
          // O chat_list usa DateTime.fromMillisecondsSinceEpoch(ts * 1000) sem isUtc
          return DateTime.fromMillisecondsSinceEpoch(timestamp);
        } else {
          // ✅ Timestamp em segundos - usar mesma lógica do chat_list
          // O chat_list usa DateTime.fromMillisecondsSinceEpoch(ts * 1000) sem isUtc
          return DateTime.fromMillisecondsSinceEpoch(timestamp * 1000);
        }
      } else if (timestamp is String) {
        // ✅ DateTime.parse() já trata ISO strings corretamente
        final parsed = DateTime.parse(timestamp);
        // ✅ Se for UTC (tem 'Z'), converter para local
        // Se não tem 'Z', assume que já está no timezone local do servidor (igual ao chat_list)
        if (timestamp.endsWith('Z')) {
          return parsed.toLocal();
        }
        // ✅ Se tem offset (+/-), DateTime.parse já ajusta automaticamente
        // Se não tem offset, assume local (timezone do servidor) - igual ao chat_list
        return parsed;
      }
    } catch (e) {
      print('❌ Erro ao parsear timestamp em tempo real: $e');
    }

    return DateTime.now();
  }

  Future<void> _loadChatHistory() async {
    if (_isLoadingHistory || _currentUserId == null) return;

    // ✅ MELHORIA: Só mostrar loading se chat estiver vazio
    final shouldShowLoading = _messages.isEmpty;
    if (shouldShowLoading) {
      setState(() => _isLoadingHistory = true);
    }

    try {
      print(' Carregando histórico (Estratégia Offline-First)...');

      // Se já há mensagens no chat, tenta atualizar do servidor
      // Se sem internet, mantém as mensagens existentes (não limpa)
      if (_messages.isNotEmpty) {
        print(' Chat já tem conteúdo, tentando atualizar do servidor...');
        try {
          final freshHistory = await ChatService.loadChatHistory(
            widget.remoteUserId,
          );

          if (mounted) {
            _processAndAddMessages(freshHistory, isLocal: false);
          }
        } catch (e) {
          print(
            ' Sem conexão com internet, mantendo mensagens existentes com status atual',
          );
          // Se sem internet, não faz nada - mantém as mensagens existentes
          // Elas já têm o status correto do último carregamento
        }
        return;
      }

      // 1. CARREGAMENTO RÁPIDO: Cache Local (só se chat estiver vazio)
      final localHistory = await ChatService.loadLocalChatHistory(
        _currentUserId!,
        widget.remoteUserId,
      );

      print(
        '🔍 DEBUG: Histórico local carregado: ${localHistory.length} mensagens',
      );
      for (int i = 0; i < localHistory.length; i++) {
        final msg = localHistory[i];
        print(
          '   Mensagem ${i + 1}: ID=${msg['message_id']}, conteúdo="${msg['content']}", de=${msg['sender_id']} para=${msg['receiver_id']}',
        );
      }

      if (mounted && localHistory.isNotEmpty) {
        _processAndAddMessages(localHistory, isLocal: true);
        // ✅ Se já tem mensagens locais, parar loading imediatamente
        if (shouldShowLoading) {
          setState(() => _isLoadingHistory = false);
        }
      }

      // ✅ 2. VERIFICAR CONEXÃO ANTES DE TENTAR SYNC
      final connectionStatus = await ChatService.checkConnectionStatus();
      final isOnline = connectionStatus == 'server_online';

      if (!isOnline) {
        print(
          '📴 Sem conexão ($connectionStatus) - mantendo mensagens locais, sem tentar sync',
        );
        // ✅ Carregar mensagens pending mesmo offline
        await _loadPendingMessagesFromStorage();
        if (shouldShowLoading) {
          setState(() => _isLoadingHistory = false);
        }
        return; // ✅ PARAR AQUI - não tentar sync se offline
      }

      // ✅ 3. CARREGAMENTO LENTO: Rede (Só se estiver online)
      print('🌐 Online - tentando sincronizar com servidor...');
      try {
        final freshHistory = await ChatService.loadChatHistory(
          widget.remoteUserId,
        );

        if (mounted) {
          _processAndAddMessages(freshHistory, isLocal: false);
        }
      } catch (e) {
        print(
          '⚠️ Erro ao sincronizar com servidor: $e - mantendo mensagens locais',
        );
        // Não é crítico - já temos mensagens locais
      }

      // ✅ NOVO: Carregar mensagens pending do sqflite DEPOIS de todos os carregamentos
      // Isso garante que as mensagens pending não sejam perdidas quando _messages é limpo
      await _loadPendingMessagesFromStorage();
    } catch (e) {
      print('❌ Erro ao carregar histórico: $e');
    } finally {
      if (mounted && shouldShowLoading) {
        setState(() => _isLoadingHistory = false);
      }
    }
  }

  // ✅ Salvar mensagem no histórico local (para persistência)
  Future<void> _saveMessageToLocalHistory(ChatMessage message) async {
    try {
      // ✅ Salvar no sqflite (pending_messages_storage)
      if (message.status == 'pending_local' || message.status == 'sent') {
        final pendingMsg = PendingMessage(
          msgId: message.id,
          to: widget.remoteUserId,
          from: _currentUserId ?? 'unknown',
          content: message.text,
          status: message.status,
          createdAt: message.timestamp,
          // ✅ Campos de reply
          replyToId: message.replyToId,
          replyToText: message.replyToText,
          replyToSenderName: message.replyToSenderName,
          replyToSenderId: message.replyToSenderId,
          // ✅ Campos de edição e deleção
          isEdited: message.isEdited,
          isDeleted: message.isDeleted,
        );
        await PendingMessagesStorage.savePendingMessage(pendingMsg);
        print('💾 Mensagem salva no storage local: ${message.id}');
      }

      // ✅ Também salvar no histórico local do ChatService (para aparecer na lista de chats)
      await ChatService.saveMessageToLocalHistory(
        _currentUserId!,
        widget.remoteUserId,
        {
          'message_id': message.id,
          'content': message.text,
          'sender_id': _currentUserId,
          'receiver_id': widget.remoteUserId,
          'sent_at': message.timestamp.toIso8601String(),
          'status': message.status,
          'is_edited': message.isEdited,
          'is_deleted': message.isDeleted,
          // ✅ Campos de reply
          'reply_to_id': message.replyToId,
          'reply_to_text': message.replyToText,
          'reply_to_sender_name': message.replyToSenderName,
          'reply_to_sender_id': message.replyToSenderId,
        },
      );
    } catch (e) {
      print('❌ Erro ao salvar mensagem no histórico local: $e');
    }
  }

  // ✅ NOVO: Listener para atualizar status em tempo real
  Timer? _statusUpdateTimer;
  // ✅ IDs de deleções iniciadas localmente (para evitar deletes fantasma)
  final Set<String> _localDeleteRequests = {};

  void _startStatusUpdateListener() {
    _statusUpdateTimer?.cancel();
    // ✅ REMOVIDO: Polling periódico de status de mensagens pendentes
    // O status é atualizado automaticamente quando:
    // - Mensagem é enviada/recebida
    // - ACK é recebido via WebSocket
    // - WebSocket reconecta
    // Não há necessidade de verificar periodicamente
  }

  // ✅ REMOVIDO: _updatePendingMessagesStatus() - polling periódico removido
  // O status é atualizado automaticamente quando:
  // - Mensagem é enviada/recebida
  // - ACK é recebido via WebSocket
  // - WebSocket reconecta
  // Não há necessidade de verificar periodicamente

  // ✅ NOVO: Carregar mensagens pending do sqflite
  Future<void> _loadPendingMessagesFromStorage() async {
    try {
      final pendingMessages = await PendingMessagesStorage.getPendingMessages(
        toUserId: widget.remoteUserId,
      );

      if (pendingMessages.isEmpty) {
        print('📭 Nenhuma mensagem pending encontrada para este chat');
        return;
      }

      print(
        '📬 Carregando ${pendingMessages.length} mensagens pending do storage...',
      );

      final pendingChatMessages = pendingMessages.map((pending) {
        return ChatMessage(
          id: pending.msgId,
          text: pending.isDeleted
              ? '⊗ Eliminou esta mensagem'
              : pending.content,
          isMe: true,
          timestamp: pending.createdAt,
          status: pending.status, // pending_local, sent, delivered, etc
          isEdited: pending.isEdited,
          isDeleted: pending.isDeleted,
          // ✅ Campos de reply carregados do sqflite
          replyToId: pending.replyToId,
          replyToText: pending.replyToText,
          replyToSenderName: pending.replyToSenderName,
          replyToSenderId: pending.replyToSenderId,
        );
      }).toList();

      if (mounted) {
        setState(() {
          // ✅ Adicionar mensagens pending que ainda não estão na lista
          for (final pendingMsg in pendingChatMessages) {
            final exists = _messages.any((msg) => msg.id == pendingMsg.id);
            if (!exists) {
              _messages.add(pendingMsg);
            }
          }
          _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
        });
        print(
          '✅ ${pendingChatMessages.length} mensagens pending adicionadas ao chat',
        );
      }
    } catch (e) {
      print('❌ Erro ao carregar mensagens pending: $e');
    }
  }

  // Helper para processar e adicionar mensagens
  void _processAndAddMessages(
    List<Map<String, dynamic>> history, {
    required bool isLocal,
  }) {
    final msgs = history.map((msg) {
      final serverTimestamp = _parseMessageTimestamp(msg);

      // ✅ Gerar ID consistente para mensagens sem message_id
      String messageId;
      if (msg['message_id'] != null && msg['message_id'].toString() != 'null') {
        messageId = msg['message_id'].toString();
      } else if (msg['id'] != null && msg['id'].toString() != 'null') {
        messageId = msg['id'].toString();
      } else {
        // ✅ Gerar ID baseado em conteúdo + timestamp + remetente para consistência
        final content = msg['content']?.toString() ?? '';
        final senderId = msg['sender_id']?.toString() ?? '';
        final timestampHash = serverTimestamp.millisecondsSinceEpoch.toString();
        messageId = 'local_${content.length}_${senderId}_$timestampHash';
      }

      return ChatMessage(
        id: messageId,
        text: _getDeletedMessageText(msg),
        isMe: _isMessageFromMe(msg),
        timestamp: serverTimestamp,
        status: (msg['status']?.toString() ?? 'sent'),
        isEdited: (msg['is_edited'] == true),
        isDeleted: (msg['is_deleted'] == true),
        replyToId: msg['reply_to_id']?.toString(),
        replyToText: msg['reply_to_text']?.toString(),
        replyToSenderName: widget.contact.name, // ✅ Sempre usar nome do contato
        replyToSenderId: msg['reply_to_sender_id']?.toString(),
      );
    }).toList();

    setState(() {
      // ✅ Só limpar se houver mensagens para adicionar
      if (msgs.isNotEmpty) {
        if (isLocal) {
          // ✅ Para carregamento local, limpar tudo (é o primeiro carregamento)
          _messages.clear();
        } else {
          // ✅ Para carregamento do servidor, remover duplicatas inteligentemente
          // Considera mudança de ID (temp → real) e conteúdo da mensagem
          _messages.removeWhere((existingMsg) {
            return msgs.any((newMsg) {
              // ✅ Mesmo ID exato
              if (newMsg.id == existingMsg.id) return true;

              // ✅ Mesmo conteúdo e mesmo remetente (possível mudança de ID)
              if (newMsg.text == existingMsg.text &&
                  newMsg.isMe == existingMsg.isMe &&
                  newMsg.timestamp.difference(existingMsg.timestamp).inSeconds <
                      60) {
                print(
                  '🔄 Removendo duplicata por conteúdo: ${existingMsg.id} → ${newMsg.id}',
                );
                return true;
              }

              return false;
            });
          });
        }

        print('🔍 DEBUG: Adicionando ${msgs.length} mensagens à UI');
        for (int i = 0; i < msgs.length; i++) {
          final msg = msgs[i];
          print(
            '   UI Mensagem ${i + 1}: ID=${msg.id}, texto="${msg.text}", isMe=${msg.isMe}',
          );
        }

        _messages.addAll(msgs);
        // ✅ Ordenar por timestamp após adicionar
        _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
        print(
          '✅ ${msgs.length} mensagens carregadas (${isLocal ? "LOCAL" : "SERVER"})',
        );
        print('🔍 DEBUG: Total de mensagens na UI agora: ${_messages.length}');
      } else {
        print(
          '⚠️ Nenhuma mensagem para carregar (${isLocal ? "LOCAL" : "SERVER"})',
        );
      }
    });

    // Scroll só se for a primeira carga ou se for server (mais confiável)
    if (isLocal || msgs.isNotEmpty) {
      _scrollToBottom();
    }
  }

  DateTime _parseMessageTimestamp(Map<String, dynamic> message) {
    try {
      final sentAt = message['sent_at'];
      DateTime parsedDateTime;

      if (sentAt is String && sentAt.contains('{{')) {
        // ✅ Erlang tuple - já está no timezone local do servidor (Moçambique)
        parsedDateTime = _parseErlangTupleTimestamp(sentAt);
      } else if (sentAt is String) {
        // ✅ DateTime.parse() já trata ISO strings corretamente
        // Se a string não tem 'Z' ou offset, assume local (timezone do servidor)
        parsedDateTime = DateTime.parse(sentAt);
        print('🔍 DEBUG String ISO: $sentAt -> parsed: $parsedDateTime');

        // ✅ Se a string não tem timezone info, tratar como local (fuso do servidor)
        if (!sentAt.contains('Z') &&
            !sentAt.contains('+') &&
            !sentAt.contains('-')) {
          // DateTime.parse() sem timezone assume UTC, então convertemos para local
          parsedDateTime = parsedDateTime.toLocal();
          print('🔍 DEBUG Convertido para local: $parsedDateTime');
        }
        // ✅ Se tem timezone info, DateTime.parse() já ajusta automaticamente
      } else if (sentAt is int) {
        // ✅ Timestamps Unix são sempre UTC - converter para local
        // Mas o chat_list usa DateTime.fromMillisecondsSinceEpoch(ts * 1000) sem isUtc
        // Vamos usar a mesma lógica do chat_list para consistência
        parsedDateTime = DateTime.fromMillisecondsSinceEpoch(sentAt * 1000);
      } else {
        final timestamp = message['timestamp'];
        if (timestamp != null && timestamp is int) {
          if (timestamp > 1000000000000) {
            // ✅ Timestamp em milissegundos - usar mesma lógica do chat_list
            // O chat_list usa DateTime.fromMillisecondsSinceEpoch(ts * 1000) sem isUtc
            parsedDateTime = DateTime.fromMillisecondsSinceEpoch(timestamp);
          } else {
            // ✅ Timestamp em segundos - usar mesma lógica do chat_list
            // O chat_list usa DateTime.fromMillisecondsSinceEpoch(ts * 1000) sem isUtc
            parsedDateTime = DateTime.fromMillisecondsSinceEpoch(
              timestamp * 1000,
            );
          }
        } else {
          throw FormatException('Nenhum formato reconhecido');
        }
      }

      return parsedDateTime;
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

        // ✅ O servidor PostgreSQL retorna timestamps no timezone do servidor (Moçambique UTC+2)
        // As Erlang tuples chegam como UTC+2, então precisamos adicionar +2 horas para exibir corretamente
        return DateTime(
          year,
          month,
          day,
          hour,
          minute,
          second,
        ).add(const Duration(hours: 2));
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
    if (text.isEmpty || _currentUserId == null) return;

    // ✅ OFFLINE-FIRST: Permitir enviar mesmo sem conexão (será salvo localmente)
    final tempMessageId =
        'temp_${DateTime.now().millisecondsSinceEpoch}_${_currentUserId}_${_uuid.v4().substring(0, 6)}';

    print('📤 Enviando mensagem: $text (ID: $tempMessageId)');

    _pendingMessageIds.add(tempMessageId);

    // ✅ OFFLINE-FIRST: Status inicial é 'pending_local' (será atualizado quando servidor confirmar)
    final initialStatus = _isConnected ? 'pending_local' : 'pending_local';

    final newMessage = ChatMessage(
      id: tempMessageId,
      text: text,
      isMe: true,
      timestamp: DateTime.now(), // ✅ Hora local real para pending
      status:
          initialStatus, // ✅ Status inicial: pending_local (será atualizado quando servidor confirmar)
      isEdited: false, // ✅ NOVA MENSAGEM NÃO É EDITADA
      isDeleted: false, // ✅ NOVA MENSAGEM NÃO É DELETADA
    );

    setState(() {
      _messages.add(newMessage);
      // ✅ ORDENAR MENSAGENS POR TIMESTAMP APÓS ADICIONAR
      _messages.sort((a, b) => a.timestamp.compareTo(b.timestamp));
    });

    // ✅ OFFLINE-FIRST: Salvar no histórico local para persistência
    await _saveMessageToLocalHistory(newMessage);

    _messageController.clear();
    _scrollToBottom();

    try {
      // ✅ OFFLINE-FIRST: ChatService.sendMessage salva localmente primeiro
      // Se não houver conexão, mensagem fica como pending_local e será sincronizada depois
      await ChatService.sendMessage(
        widget.remoteUserId,
        text,
        tempId: tempMessageId,
      );

      // ✅ Status será atualizado automaticamente quando receber confirmação do servidor
      // (via _handleIncomingMessage quando receber ACK com db_message_id)
    } catch (e) {
      print('❌ Falha ao enviar mensagem: $e');
      // ✅ Mensagem já está salva localmente como pending_local
      // Não remover da UI - ela será sincronizada automaticamente quando conexão voltar

      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          const SnackBar(
            content: Text(
              'Mensagem salva localmente. Será enviada quando conexão voltar.',
            ),
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
                    _buildMarqueeText()
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
                    physics: const ClampingScrollPhysics(),
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

  // 🎤 CONTROLES DE ÁUDIO
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
        child: SingleChildScrollView(
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
                      style: TextStyle(
                        fontSize: 18,
                        fontWeight: FontWeight.w600,
                      ),
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
      final editingId = _editingMessageId!;
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

              // 🔍 DEBUG: VERIFICAR STATUS ANTES DE MUDAR
              print('🔍 DEBUG EDIÇÃO:');
              print('   Status delivery original: ${oldMessage.status}');
              print('   Status backend: ${updatedMessage['status']}');
              print('   is_edited backend: ${updatedMessage['is_edited']}');
              print('   Status delivery que será usado: ${oldMessage.status}');

              _messages[messageIndex] = ChatMessage(
                id: updatedMessage['id'].toString(),
                text: updatedMessage['content'],
                isMe: oldMessage.isMe,
                timestamp:
                    oldMessage.timestamp, // ✅ PRESERVAR TIMESTAMP ORIGINAL!
                status: oldMessage
                    .status, // ✅ PRESERVAR STATUS DELIVERY (sent/delivered/read)
                isEdited: true, // ✅ MARCAR COMO EDITADA
                isDeleted:
                    oldMessage.isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
                // ✅ PRESERVAR DADOS DE REPLY
                replyToId: oldMessage.replyToId,
                replyToText: oldMessage.replyToText,
                replyToSenderName: oldMessage.replyToSenderName,
                replyToSenderId: oldMessage.replyToSenderId,
              );

              // 🔍 DEBUG: VERIFICAR STATUS APÓS MUDAR
              print(
                '   ✅ Status delivery após edição: ${_messages[messageIndex].status}',
              );

              // ✅ ATUALIZAR CONTEÚDO NO HISTÓRICO LOCAL (preservar status delivery)
              final historyMessageIndex = _messageHistory.indexWhere(
                (msg) => msg['message_id']?.toString() == _editingMessageId,
              );
              if (historyMessageIndex != -1) {
                _messageHistory[historyMessageIndex] = {
                  ..._messageHistory[historyMessageIndex],
                  'content':
                      updatedMessage['content'], // ✅ ATUALIZAR APENAS CONTEÚDO
                  'is_edited':
                      updatedMessage['is_edited'], // ✅ ATUALIZAR is_edited
                  // ✅ NÃO MUDAR STATUS DELIVERY - PRESERVAR ORIGINAL!
                };
              }
            }
            _editingMessageId = null;
            _selectedMessageId = null; // ✅ LIMPAR TAMBÉM O SELECTED MESSAGE ID
            _editController.clear();
            _messageController.clear();

            print(
              '✅ Conteúdo atualizado no histórico local (status delivery preservado)',
            );
          });

          // ✅ OFFLINE-FIRST: Atualizar no sqflite se for mensagem pending
          final pendingMsg = await PendingMessagesStorage.getMessageById(
            editingId,
          );
          if (pendingMsg != null) {
            await PendingMessagesStorage.updateMessageContent(
              editingId,
              result['edited_message']['content'],
            );
            print('💾 Edição salva no sqflite: $editingId');
          }

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
      _localDeleteRequests.add(message.id);

      // ✅ OFFLINE-FIRST: Atualizar localmente primeiro
      setState(() {
        final messageIndex = _messages.indexWhere(
          (msg) => msg.id == message.id,
        );
        if (messageIndex != -1) {
          final oldMessage = _messages[messageIndex];
          _messages[messageIndex] = ChatMessage(
            id: oldMessage.id,
            text: '⊗ Eliminou esta mensagem',
            isMe: oldMessage.isMe,
            timestamp: oldMessage.timestamp,
            status: oldMessage.status,
            isEdited: oldMessage.isEdited,
            isDeleted: true, // ✅ MARCAR COMO DELETADA
            replyToId: oldMessage.replyToId,
            replyToText: oldMessage.replyToText,
            replyToSenderName: oldMessage.replyToSenderName,
            replyToSenderId: oldMessage.replyToSenderId,
          );
        }
      });

      // ✅ OFFLINE-FIRST: Atualizar no sqflite se for mensagem pending
      final pendingMsg = await PendingMessagesStorage.getMessageById(
        message.id,
      );
      if (pendingMsg != null) {
        await PendingMessagesStorage.markMessageAsDeleted(message.id);
        print('💾 Deleção salva no sqflite: ${message.id}');
      }

      // Chamar backend para deletar mensagem
      try {
        final result = await MessageOperationsService.deleteMessage(message.id);
        if (result['success'] == true) {
          print('🗑️ Mensagem apagada no servidor: ${message.id}');
        }
      } catch (e) {
        print('⚠️ Erro ao apagar no servidor (mas salvo localmente): $e');
        // ✅ Mensagem já está marcada como deletada localmente
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
          isDeleted: false, // ✅ PADRÃO: NÃO DELETADA
        );
      }

      // ✅ 2. CRIAR ID TEMPORÁRIO PARA A RESPOSTA
      final tempReplyId =
          'temp_reply_${DateTime.now().millisecondsSinceEpoch}_${_uuid.v4().substring(0, 8)}';

      print('   ID Temporário: $tempReplyId');

      // ✅ 3. CRIAR MENSAGEM LOCAL COM INFORMAÇÕES COMPLETAS
      // ✅ OFFLINE-FIRST: Status inicial é 'pending_local' (será atualizado quando servidor confirmar)
      final initialStatus = 'pending_local';

      final localReply = ChatMessage(
        id: tempReplyId,
        text: replyText,
        isMe: true,
        timestamp: DateTime.now().add(
          const Duration(hours: 2),
        ), // ✅ Já no fuso de Moçambique
        status: initialStatus, // ✅ Status inicial: pending_local
        isEdited: false, // ✅ NOVA MENSAGEM NÃO É EDITADA
        isDeleted: false, // ✅ NOVA MENSAGEM NÃO É DELETADA
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

      // ✅ 5. OFFLINE-FIRST: Salvar no histórico local e sqflite
      await _saveMessageToLocalHistory(localReply);

      // ✅ 6. SCROLL PARA BAIXO
      _scrollToBottom();

      // ✅ 7. ADICIONAR À LISTA DE PENDENTES (PARA EVITAR DUPLICAÇÃO)
      _pendingMessageIds.add(tempReplyId);

      // ✅ 8. ENVIAR PARA O BACKEND - USAR VARIÁVEL LOCAL SALVA
      print('🔄 Enviando reply para o backend...');
      try {
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
            // ✅ 9. ATUALIZAR MENSAGEM LOCAL COM ID REAL DO BANCO
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
                  isEdited: _messages[messageIndex]
                      .isEdited, // ✅ PRESERVAR STATUS DE EDIÇÃO!
                  isDeleted: _messages[messageIndex]
                      .isDeleted, // ✅ PRESERVAR STATUS DE DELEÇÃO!
                  replyToId: _messages[messageIndex].replyToId,
                  replyToText: _messages[messageIndex].replyToText,
                  replyToSenderName: _messages[messageIndex].replyToSenderName,
                  replyToSenderId: _messages[messageIndex].replyToSenderId,
                );
              });
            }

            // ✅ Atualizar status no storage + histórico (e limpar pending)
            await ChatService.updateMessageStatusFromServer(
              tempReplyId,
              replyMessage['status']?.toString() ?? 'sent',
              dbMessageId: dbMessageId,
              sentAt: replyMessage['sent_at']?.toString(),
            );

            _pendingMessageIds.remove(tempReplyId);
          } else {
            print('⚠️ Reply enviado mas dbMessageId é nulo');
          }

          // ✅ 10. ATUALIZAR CHAT LIST
          ChatService.updateChatAfterReply(widget.remoteUserId, replyText);

          print('✅ Reply processado com sucesso!');
        } else {
          print('❌ ERRO NO BACKEND AO ENVIAR REPLY: ${result['error']}');
        }
      } catch (e) {
        print('❌ Falha ao enviar reply: $e');
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
                          fontSize: 12,
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
                  constraints: BoxConstraints(maxWidth: double.infinity),
                  decoration: BoxDecoration(
                    color: AppTheme.inputBackground,
                    borderRadius: BorderRadius.circular(25),
                    border: Border.all(color: AppTheme.inputBorder),
                  ),
                  child: TextField(
                    keyboardType: TextInputType.multiline,
                    maxLines: 6,
                    minLines: 1,
                    textInputAction: TextInputAction.newline,
                    controller: _messageController,
                    decoration: InputDecoration(
                      hintText: _editingMessageId != null
                          ? 'Editando mensagem...'
                          : (_selectedMessageId != null
                                ? 'Sua resposta...'
                                : 'Mensagem'),
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
                  ),
                ),
              ),
              const SizedBox(width: 8),
              Container(
                decoration: BoxDecoration(
                  // ✅ OFFLINE-FIRST: Sempre verde, independente de conexão
                  color: AppTheme.appBarColor,
                  shape: BoxShape.circle,
                ),
                child: IconButton(
                  icon: Icon(
                    _messageController.text.trim().isEmpty
                        ? (_isRecording ? Icons.stop : Icons.mic)
                        : Icons.send,
                    color: AppTheme.textOnGreen,
                  ),
                  // ✅ OFFLINE-FIRST: Permitir enviar mesmo sem conexão (será salvo localmente)
                  onPressed: () {
                    if (_editingMessageId != null) {
                      _updateMessage();
                    } else if (_selectedMessageId != null) {
                      _sendReply();
                    } else if (_messageController.text.trim().isEmpty) {
                      _toggleVoiceRecording();
                    } else {
                      _sendMessage();
                    }
                  },
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

    final isOwnMessage = message.isMe;
    final replyIsOwn = message.replyToSenderId == _currentUserId.toString();
    final replySenderName = replyIsOwn
        ? 'Eu'
        : (message.replyToSenderName ?? 'Desconhecido');

    // Cores baseadas no tipo de balão (Enviado vs Recebido)
    final backgroundColor = isOwnMessage
        ? Colors.black.withOpacity(0.1) // Escurece levemente o verde
        : const Color(0xFFF5F5F5).withOpacity(0.6); // Cinza no balão branco

    final textColor = isOwnMessage
        ? Colors.white.withOpacity(0.9)
        : Colors.black.withOpacity(0.6);

    // DEFINIÇÃO DE CORES DE DESTAQUE (Barra e Nome)
    Color accentColor;

    if (isOwnMessage) {
      // ESTAMOS NO BALÃO VERDE (Enviado) -> Precisamos de cores Claras
      if (replyIsOwn) {
        // Respondendo a mim mesmo: "Você" em Branco para máximo contraste
        accentColor = Colors.white;
      } else {
        // Respondendo a outro: Nome dele. O Roxo escuro não aparece no verde.
        // Usamos um Roxo Claro/Lilás ou Laranja que contraste bem com verde escuro.
        accentColor = const Color(0xFFE1BEE7); // Purple 100 (Lilás claro)
      }
    } else {
      // ESTAMOS NO BALÃO BRANCO (Recebido) -> Cores Escuras normais
      if (replyIsOwn) {
        // Respondendo a mim: Verde escuro
        accentColor = AppTheme.appBarColor;
      } else {
        // Respondendo a outro: Roxo escuro
        accentColor = const Color(0xFF6B4B9E);
      }
    }

    return Container(
      margin: const EdgeInsets.only(bottom: 0), // Margem 0 para colar no texto
      padding: EdgeInsets.all(4),
      decoration: BoxDecoration(
        color: backgroundColor,
        borderRadius: BorderRadius.circular(6),
        // Gambiarra visual para a borda esquerda ficar dentro do arredondamento:
        // Usamos um container interno recortado ou apenas BorderSide se funcionar bem.
        // O WhatsApp usa radius pequeno (4-6).
      ),
      child: IntrinsicHeight(
        child: Row(
          mainAxisSize: MainAxisSize
              .min, // ✅ IMPORTANTE: Ocupar apenas o espaço necessário
          children: [
            Container(
              width: 4,
              decoration: BoxDecoration(
                color: accentColor,
                borderRadius: BorderRadius.vertical(
                  top: Radius.circular(2),
                  bottom: Radius.circular(2),
                ),
              ),
            ),
            SizedBox(width: 8),
            Flexible(
              // ✅ Usar Flexible em vez de Expanded para permitir encolher
              fit: FlexFit.loose,
              child: Column(
                crossAxisAlignment: CrossAxisAlignment.start,
                mainAxisAlignment: MainAxisAlignment.center,
                mainAxisSize: MainAxisSize.min,
                children: [
                  Text(
                    replySenderName,
                    style: TextStyle(
                      color: accentColor,
                      fontWeight: FontWeight.bold,
                      fontSize: 13,
                    ),
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                  ),
                  const SizedBox(height: 2),
                  Text(
                    message.replyToText!,
                    style: TextStyle(color: textColor, fontSize: 13),
                    maxLines: 3,
                    overflow: TextOverflow.ellipsis,
                  ),
                ],
              ),
            ),
            SizedBox(width: 4),
          ],
        ),
      ),
    );
  }

  Widget _buildMessageBubble(ChatMessage message) {
    return GestureDetector(
      onLongPress: () => _showMessageOptions(message),
      child: Padding(
        padding: const EdgeInsets.symmetric(vertical: 4),
        child: Align(
          alignment: message.isMe
              ? Alignment.centerRight
              : Alignment.centerLeft,
          child: ConstrainedBox(
            constraints: BoxConstraints(
              maxWidth: MediaQuery.of(context).size.width * 0.80,
              minWidth: 100, // Garantir largura mínima para hora
            ),
            child: CustomPaint(
              painter: BubblePainter(
                color: message.isDeleted
                    ? Colors.grey[200]!
                    : (message.isMe
                          ? AppTheme
                                .appBarColor // Cor exata da AppBar
                          : AppTheme.messageReceived),
                alignment: message.isMe
                    ? Alignment.topRight
                    : Alignment.topLeft,
                tail: true,
              ),
              child: Container(
                margin: EdgeInsets.fromLTRB(
                  message.isMe
                      ? 8
                      : 16, // Margem esquerda (maior se recebido para tail)
                  4,
                  message.isMe
                      ? 16
                      : 8, // Margem direita (maior se enviado para tail)
                  4,
                ),
                child: Stack(
                  children: [
                    Padding(
                      padding: const EdgeInsets.only(
                        left: 3,
                        right: 3,
                        top: 2,
                        bottom: 18,
                      ),
                      child: IntrinsicWidth(
                        child: Column(
                          crossAxisAlignment: CrossAxisAlignment.stretch,
                          mainAxisSize: MainAxisSize.min,
                          children: [
                            if (!message.isDeleted) _buildReplyPreview(message),
                            Padding(
                              padding: const EdgeInsets.only(
                                left: 2,
                                top: 1,
                              ), // Top 1 para "quase colar", mas com leve respiro
                              child: Text(
                                message.isDeleted ? message.text : message.text,
                                style: TextStyle(
                                  color: message.isDeleted
                                      ? Colors.grey[600]
                                      : (message.isMe
                                            ? AppTheme.messageSentText
                                            : AppTheme.messageReceivedText),
                                  fontSize: 16,
                                  fontWeight: FontWeight.normal,
                                  fontStyle: message.isDeleted
                                      ? FontStyle.italic
                                      : FontStyle.normal,
                                ),
                              ),
                            ),
                          ],
                        ),
                      ),
                    ),
                    Positioned(
                      bottom: 2, // Subir a hora (de 4 para 2)
                      right: 8, // Ajuste lateral leve
                      child: Row(
                        mainAxisSize: MainAxisSize.min,
                        children: [
                          Text(
                            _formatTime(
                              message.timestamp,
                              isPending: message.status == 'pending_local',
                            ),
                            style: TextStyle(
                              color: message.isDeleted
                                  ? Colors.grey[600]
                                  : (message.isMe
                                        ? AppTheme.messageSentText.withOpacity(
                                            0.7,
                                          )
                                        : Colors.grey[600]),
                              fontSize: 11,
                            ),
                          ),
                          if (message.isEdited && !message.isDeleted) ...[
                            const SizedBox(width: 4),
                            Icon(
                              Icons.edit,
                              size: 10,
                              color: message.isMe
                                  ? AppTheme.messageSentText.withOpacity(0.7)
                                  : Colors.grey[600],
                            ),
                          ],
                          if (message.isMe && !message.isDeleted) ...[
                            const SizedBox(width: 4),
                            _buildStatusIcon(message.status),
                          ],
                        ],
                      ),
                    ),
                  ],
                ),
              ),
            ),
          ),
        ),
      ),
    );
  }

  String _formatTime(DateTime timestamp, {bool isPending = false}) {
    // Mensagens pending já vêm com timestamp no fuso correto, não adicionar +2 horas
    // Outras mensagens também já vêm com timestamp no fuso correto do servidor
    final adjustedTime = timestamp;

    // Todas as mensagens devem mostrar a hora para facilitar auditoria e leitura
    return '${adjustedTime.hour.toString().padLeft(2, '0')}:${adjustedTime.minute.toString().padLeft(2, '0')}';
  }

  Widget _buildStatusIcon(String status) {
    IconData icon;
    Color color;

    switch (status) {
      case 'pending_local':
        // ✅ Ícone de relógio para mensagens pendentes (offline)
        icon = Icons.access_time;
        color = AppTheme.statusSent.withOpacity(0.5); // Cinza claro
        break;
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

class BubblePainter extends CustomPainter {
  final Color color;
  final Alignment alignment;
  final bool tail;

  BubblePainter({
    required this.color,
    required this.alignment,
    required this.tail,
  });

  final double _radius = 10.0;
  final double _x = 10.0;

  @override
  void paint(Canvas canvas, Size size) {
    if (alignment == Alignment.topRight) {
      // Desenho oficial WhatsApp-like (Sent)
      // Com cauda somente, sem vértices estranhos no topo
      final w = size.width;
      final h = size.height;
      var sentPath = Path();
      sentPath.moveTo(_radius, 0); // Começa arredondado top-left
      sentPath.lineTo(w - _radius, 0); // Vai até top-right (antes da curva)

      // Curva top-right normal (arredondada, sem bico)
      sentPath.quadraticBezierTo(w, 0, w, _radius);

      sentPath.lineTo(w, h - _radius); // Desce até bottom-right

      // Inicio cauda no bottom-right
      sentPath.quadraticBezierTo(w, h, w + 10, h); // Ponta da cauda
      sentPath.lineTo(w - 10, h); // Volta para a base do balão
      sentPath.quadraticBezierTo(
        w - 10,
        h,
        w - 10,
        h,
      ); // (Redundante, mas mantendo estrutura)

      sentPath.lineTo(_radius, h); // Linha inferior até bottom-left
      sentPath.quadraticBezierTo(0, h, 0, h - _radius); // Curva bottom-left
      sentPath.lineTo(0, _radius); // Sobe esquerda
      sentPath.quadraticBezierTo(0, 0, _radius, 0); // Curva top-left

      canvas.drawPath(sentPath, Paint()..color = color);
    } else {
      // Received
      var path = Path();
      final w = size.width;
      final h = size.height;

      path.moveTo(_radius, 0);
      path.lineTo(w - _radius, 0);
      path.quadraticBezierTo(w, 0, w, _radius);
      path.lineTo(w, h - _radius);
      path.quadraticBezierTo(w, h, w - _radius, h);
      path.lineTo(_radius + 10, h); // Antes da cauda esquerd
      // Cauda esquerda
      path.quadraticBezierTo(0, h, -10, h); // Ponta esquerda
      path.quadraticBezierTo(0, h, 0, h - _radius); // Volta para cima

      path.lineTo(0, _radius);
      path.quadraticBezierTo(0, 0, _radius, 0);

      canvas.drawPath(path, Paint()..color = color);
    }
  }

  @override
  bool shouldRepaint(CustomPainter oldDelegate) {
    return true;
  }
}
