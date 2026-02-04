import 'package:flutter/material.dart';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:url_launcher/url_launcher.dart';
import 'auth_service.dart';
import 'chat_page.dart';
import 'chat_service.dart';
import 'chat_model.dart';
import 'notification_service.dart';
import 'app_theme.dart';
import 'dart:async';
import 'contacts_helper.dart';

class ChatListPage extends StatefulWidget {
  @override
  _ChatListPageState createState() => _ChatListPageState();
}

class _ChatListPageState extends State<ChatListPage>
    with TickerProviderStateMixin {
  Timer? _sessionTimer;
  Timer? _chatRefreshTimer;
  late TabController _tabController;
  final TextEditingController _searchController = TextEditingController();
  String _searchQuery = '';

  // ✅ SISTEMA DE CHATS REAL
  StreamSubscription<List<ChatContact>>? _chatSubscription;
  StreamSubscription<Map<String, dynamic>>? _messageSubscription;
  StreamSubscription<Map<String, dynamic>>? _typingSubscription;
  List<ChatContact> _chats = [];
  bool _isLoading = true; // ✅ Controle de loading inicial
  final Map<String, bool> _typingUsers = {};
  final Map<String, Timer> _typingTimers = {};

  // ✅ ESTADO DE SELEÇÃO
  final Set<String> _selectedChatIds = {};
  bool get _isSelectionMode => _selectedChatIds.isNotEmpty;

  void _toggleSelection(String contactId) {
    setState(() {
      if (_selectedChatIds.contains(contactId)) {
        _selectedChatIds.remove(contactId);
      } else {
        _selectedChatIds.add(contactId);
      }
    });
  }

  void _clearSelection() {
    setState(() {
      _selectedChatIds.clear();
    });
  }

  @override
  void initState() {
    super.initState();
    _tabController = TabController(length: 3, vsync: this);
    _startSessionValidationTimer();
    _initializeRealChats();
    _setupTypingListener();
  }

  @override
  void dispose() {
    _sessionTimer?.cancel();
    _chatRefreshTimer?.cancel();
    _chatSubscription?.cancel();
    _messageSubscription?.cancel();
    _typingSubscription?.cancel();
    _typingTimers.values.forEach((timer) => timer.cancel());
    _tabController.dispose();
    _searchController.dispose();
    super.dispose();
  }

  // ✅ APP BAR NORMAL
  AppBar _buildNormalAppBar() {
    return AppBar(
      backgroundColor: AppTheme.appBarColor,
      elevation: 0,
      title: Text(
        'SpeekJoy',
        style: TextStyle(
          color: AppTheme.textOnGreen,
          fontSize: 20,
          fontWeight: FontWeight.w500,
        ),
      ),
      actions: [
        // BOTÃO DE RECARREGAR CHATS
        IconButton(
          icon: _isLoading
              ? SizedBox(
                  width: 20,
                  height: 20,
                  child: CircularProgressIndicator(
                    strokeWidth: 2,
                    valueColor: AlwaysStoppedAnimation<Color>(
                      AppTheme.textOnGreen,
                    ),
                  ),
                )
              : Icon(Icons.refresh, color: AppTheme.textOnGreen),
          onPressed: _isLoading ? null : _reloadChats,
        ),
        IconButton(
          icon: Icon(Icons.camera_alt, color: AppTheme.textOnGreen),
          onPressed: () {},
        ),
        IconButton(
          icon: Icon(Icons.search, color: AppTheme.textOnGreen),
          onPressed: () {},
        ),
        PopupMenuButton<String>(
          icon: Icon(Icons.more_vert, color: AppTheme.textOnGreen),
          onSelected: (value) {
            if (value == 'logout')
              _logout();
            else if (value == 'revoke_others')
              _showRevokeDialog();
          },
          itemBuilder: (context) => [
            PopupMenuItem(
              value: 'revoke_others',
              child: Row(
                children: [
                  Icon(Icons.security, color: AppTheme.actionDelete),
                  SizedBox(width: 8),
                  Text('Sair de outros dispositivos'),
                ],
              ),
            ),
            PopupMenuItem(
              value: 'logout',
              child: Row(
                children: [
                  Icon(Icons.logout, color: Colors.red),
                  SizedBox(width: 8),
                  Text('Logout'),
                ],
              ),
            ),
          ],
        ),
      ],
      bottom: PreferredSize(
        preferredSize: Size.fromHeight(70),
        child: Container(
          color: AppTheme.surfaceColor,
          child: Column(
            children: [
              Container(height: 1, color: AppTheme.dividerColor),
              Container(
                padding: EdgeInsets.symmetric(horizontal: 8, vertical: 12),
                child: TabBar(
                  controller: _tabController,
                  indicatorColor: AppTheme.appBarColor,
                  labelColor: AppTheme.appBarColor,
                  unselectedLabelColor: AppTheme.textSecondary,
                  tabs: [
                    Tab(
                      child: Column(
                        children: [
                          Icon(Icons.chat_bubble_outline, size: 18),
                          SizedBox(height: 2),
                          Text('CHATS', style: TextStyle(fontSize: 10)),
                        ],
                      ),
                    ),
                    Tab(
                      child: Column(
                        children: [
                          Icon(Icons.radio_button_checked, size: 18),
                          SizedBox(height: 2),
                          Text('STATUS', style: TextStyle(fontSize: 10)),
                        ],
                      ),
                    ),
                    Tab(
                      child: Column(
                        children: [
                          Icon(Icons.call, size: 18),
                          SizedBox(height: 2),
                          Text('CALLS', style: TextStyle(fontSize: 10)),
                        ],
                      ),
                    ),
                  ],
                ),
              ),
            ],
          ),
        ),
      ),
    );
  }

  // ✅ APP BAR DE SELEÇÃO
  AppBar _buildSelectionAppBar() {
    return AppBar(
      backgroundColor: AppTheme.appBarColor,
      leading: IconButton(
        icon: Icon(Icons.arrow_back, color: AppTheme.textOnGreen),
        onPressed: _clearSelection,
      ),
      title: Text(
        '${_selectedChatIds.length}',
        style: TextStyle(
          color: AppTheme.textOnGreen,
          fontSize: 18,
          fontWeight: FontWeight.bold,
        ),
      ),
      actions: [
        IconButton(
          icon: Icon(Icons.push_pin, color: AppTheme.textOnGreen),
          onPressed: () {}, // Fixar (Placeholder)
        ),
        IconButton(
          icon: Icon(Icons.delete, color: AppTheme.textOnGreen),
          onPressed: () {
            // Deletar selecionados
            if (_selectedChatIds.isNotEmpty) {
              final chat = _chats.firstWhere(
                (c) => c.contactId == _selectedChatIds.first,
              );
              _showDeleteChatDialog(chat);
            }
          },
        ),
        IconButton(
          icon: Icon(Icons.volume_off, color: AppTheme.textOnGreen),
          onPressed: () {}, // Mute (Placeholder)
        ),
        IconButton(
          icon: Icon(Icons.archive, color: AppTheme.textOnGreen),
          onPressed: () {}, // Arquivar (Placeholder)
        ),
        PopupMenuButton<String>(
          icon: Icon(Icons.more_vert, color: AppTheme.textOnGreen),
          itemBuilder: (context) => [
            PopupMenuItem(value: 'view_contact', child: Text('Ver contacto')),
            PopupMenuItem(
              value: 'add_favorite',
              child: Text('Adicionar aos favoritos'),
            ),
            PopupMenuItem(value: 'lock_chat', child: Text('Trancar conversa')),
            PopupMenuItem(value: 'add_list', child: Text('Adicionar a lista')),
            PopupMenuItem(value: 'block', child: Text('Bloquear')),
          ],
        ),
      ],
    );
  }

  // 🔔 TRATAR MENSAGENS GLOBAIS PARA NOTIFICAÇÕES
  void _handleGlobalMessage(Map<String, dynamic> message) async {
    final type = message['type']?.toString();

    // Ignorar eventos de status (delivered, read)
    if (type == 'message_delivered' || type == 'message_read') {
      return;
    }

    final fromUserId = message['from']?.toString();
    final toUserId = message['to']?.toString();
    final content = message['content']?.toString() ?? '';

    // Verificar se é uma mensagem recebida (não enviada por mim)
    if (fromUserId != null && toUserId != null && content.isNotEmpty) {
      final currentUserId = await AuthService.getCurrentUserId();

      // Se eu sou o destinatário e não estou no chat do remetente
      if (toUserId == currentUserId &&
          ChatService.activeChatContactId != fromUserId) {
        _sendNotificationForMessage(fromUserId, content);
      }
    }
  }

  // 🔔 ENVIAR NOTIFICAÇÃO DE MENSAGEM
  void _sendNotificationForMessage(
    String senderId,
    String messageContent,
  ) async {
    try {
      // Buscar informações do remetente
      final senderChat = _chats.firstWhere(
        (chat) => chat.contactId == senderId,
        orElse: () => ChatContact(
          contactId: senderId,
          name: 'Desconhecido',
          lastMessage: '',
          lastMessageTime: DateTime.now(),
          unreadCount: 0,
        ),
      );

      await NotificationService().showNewMessageNotification(
        senderName: senderChat.name,
        messageContent: messageContent,
        chatId: senderId,
      );

      print('🔔 Notificação enviada: ${senderChat.name} - $messageContent');
    } catch (e) {
      print('❌ Erro ao enviar notificação: $e');
    }
  }

  // INICIALIZAR CHATS REAIS - CORRIGIDO
  void _initializeRealChats() {
    print('🚀 Inicializando chats reais...');

    // CONECTAR AO WEBSOCKET PRIMEIRO (AGORA: DEPOIS DO LOCAL)
    WidgetsBinding.instance.addPostFrameCallback((_) async {
      print('🚀 Inicializando chats: Verificando pré-carregamento...');

      // ✅ 1. VERIFICAR SE JÁ TEMOS DADOS (Do main.dart)
      var currentChats = ChatService.currentChatList;

      if (currentChats.isNotEmpty) {
        print('✅ Usando dados pré-carregados do main: ${currentChats.length}');
        if (mounted) {
          setState(() {
            _chats = currentChats;
            _isLoading = false;
          });
        }
      } else {
        // Se não tiver, aí sim carrega do disco
        print('📂 Sem pré-carregamento, lendo do disco...');
        await ChatService.loadLocalChats();
        currentChats = ChatService.currentChatList;

        if (mounted) {
          setState(() {
            _chats = currentChats;
            _isLoading = false;
          });
        }
      }
      print('📊 Chats locais exibidos: ${_chats.length}');

      if (_chats.isEmpty) {
        // Se ainda estiver vazio APÓS carregar local, então mostrar estado vazio
        if (mounted) {
          setState(() {
            _isLoading = false;
          });
        }
        print('🔄 Nenhum chat local, tentando reconstruir do histórico...');
        await ChatService.rebuildChatsFromHistory();
      }

      // 2. CONECTAR EM BACKGROUND (sem await para não bloquear UI)
      print('🔌 Iniciando conexão WebSocket em background...');
      ChatService.connect().then((_) {
        // Opcional: Se precisar fazer algo após conectar
      });

      print('📊 Chats iniciais carregados: ${_chats.length}');

      // SE NÃO HOUVER CHATS, TENTAR RECONSTRUIR
      if (_chats.isEmpty) {
        print('🔄 Nenhum chat encontrado, reconstruindo...');
        await ChatService.rebuildChatsFromHistory();
      }

      // INICIAR TIMER DE VERIFICAÇÃO
      _startChatRefreshTimer();
    });

    // OUVIR ATUALIZAÇÕES EM TEMPO REAL
    _chatSubscription = ChatService.chatListStream.listen((chats) {
      if (mounted) {
        setState(() {
          _chats = chats;
        });
      }
    });

    // 🔔 OUVIR MENSAGENS GLOBAIS PARA NOTIFICAÇÕES
    _messageSubscription = ChatService.messageStream.listen((message) {
      _handleGlobalMessage(message);
    });
  }

  // VERIFICAR E RECARREGAR CHATS PERIODICAMENTE
  void _startChatRefreshTimer() {
    _chatRefreshTimer = Timer.periodic(Duration(seconds: 3), (timer) {
      if (_chats.isEmpty && mounted) {
        print('🔄 Verificando chats vazios...');
        // Não força reconstrução automática - deixa o usuário recarregar manualmente
      }
    });
  }

  void _startSessionValidationTimer() {
    _sessionTimer = Timer.periodic(Duration(seconds: 10), (timer) async {
      final isValid = await AuthService.validateCurrentSession();

      // ✅ INTELIGÊNCIA DE RECONEXÃO:
      // Apenas se isValid == true (Server Online e 200 OK) E não estiver conectado
      if (isValid == true &&
          ChatService.isServerDown &&
          !ChatService.isConnected) {
        print(
          '🌍 Servidor detectado ONLINE (HTTP OK). Tentando reconectar WebSocket...',
        );
        ChatService.connect();
      }

      // ✅ Apenas faz logout se isValid == false (Sessão Revogada).
      // Se for null (offline), ignora.
      if (isValid == false && mounted) {
        timer.cancel();
        _showSessionExpiredDialog();
      }
    });
  }

  void _showSessionExpiredDialog() {
    showDialog(
      context: context,
      barrierDismissible: false,
      builder: (context) => AlertDialog(
        title: Text('Sessão Expirada'),
        content: Text(
          'Sua sessão foi encerrada em outro dispositivo. Você será redirecionado para o login.',
        ),
        actions: [
          TextButton(
            onPressed: () {
              _redirectToLogin();
            },
            child: Text('OK'),
          ),
        ],
      ),
    );
  }

  void _redirectToLogin() async {
    await AuthService.clearLocalSession();
    Navigator.pushNamedAndRemoveUntil(context, '/welcome', (route) => false);
  }

  // BOTÃO PARA RECARREGAR CONVERSAS
  Future<void> _reloadChats() async {
    setState(() => _isLoading = true);

    print('🔄 Recarregando conversas...');

    // FORÇAR RECONSTRUÇÃO DOS CHATS
    await ChatService.rebuildChatsFromHistory();

    if (mounted) {
      setState(() => _isLoading = false);
    }
  }

  // ABRIR LISTA DE CONTATOS
  Future<void> _openContactsList() async {
    final contactPermission = await Permission.contacts.status;

    if (!contactPermission.isGranted) {
      final result = await Permission.contacts.request();
      if (!result.isGranted) {
        _showPermissionDeniedDialog();
        return;
      }
    }

    setState(() => _isLoading = true);
    try {
      final contacts = await FlutterContacts.getContacts(
        withProperties: true,
        withPhoto: true,
      );

      _showContactsSelectionDialog(contacts);
    } catch (e) {
      ScaffoldMessenger.of(
        context,
      ).showSnackBar(SnackBar(content: Text('Erro ao carregar contatos: $e')));
    } finally {
      setState(() => _isLoading = false);
    }
  }

  // DIÁLOGO DE SELEÇÃO DE CONTATOS (mantido igual)
  void _showContactsSelectionDialog(List<Contact> contacts) {
    final searchController = TextEditingController();
    List<Contact> filteredContacts = List.from(contacts);
    bool _isSearching = false;

    showDialog(
      context: context,
      builder: (context) => StatefulBuilder(
        builder: (context, setState) {
          void filterContacts(String query) {
            setState(() {
              if (query.isEmpty) {
                filteredContacts = List.from(contacts);
              } else {
                filteredContacts = contacts.where((contact) {
                  final name = contact.displayName.toLowerCase();
                  final phones = contact.phones
                      .map((phone) => phone.number)
                      .join(' ');
                  final searchLower = query.toLowerCase();

                  return name.contains(searchLower) ||
                      phones.contains(searchLower) ||
                      phones
                          .replaceAll(RegExp(r'[\s\-\(\)]'), '')
                          .contains(searchLower);
                }).toList();
              }
            });
          }

          void _toggleSearch() {
            setState(() {
              _isSearching = !_isSearching;
              if (!_isSearching) {
                searchController.clear();
                filterContacts('');
              }
            });
          }

          return AlertDialog(
            title: Container(
              height: 40,
              child: Row(
                mainAxisAlignment: MainAxisAlignment.spaceBetween,
                children: [
                  Row(
                    children: [
                      Icon(
                        Icons.contacts,
                        color: AppTheme.appBarColor,
                        size: 20,
                      ),
                      SizedBox(width: 6),
                      if (!_isSearching)
                        Text(
                          'Selecionar Contacto',
                          style: TextStyle(
                            fontSize: 16,
                            fontWeight: FontWeight.w500,
                          ),
                        ),
                    ],
                  ),

                  if (_isSearching)
                    Expanded(
                      child: Row(
                        children: [
                          Expanded(
                            child: Padding(
                              padding: EdgeInsets.only(left: 16),
                              child: TextField(
                                controller: searchController,
                                autofocus: true,
                                onChanged: filterContacts,
                                decoration: InputDecoration(
                                  hintText: 'Pesquisar...',
                                  hintStyle: TextStyle(
                                    color: AppTheme.textSecondary,
                                    fontSize: 14,
                                  ),
                                  border: InputBorder.none,
                                  contentPadding: EdgeInsets.zero,
                                ),
                                style: TextStyle(fontSize: 14),
                              ),
                            ),
                          ),
                          IconButton(
                            icon: Icon(
                              Icons.close,
                              color: AppTheme.appBarColor,
                              size: 20,
                            ),
                            padding: EdgeInsets.all(4),
                            constraints: BoxConstraints(
                              minWidth: 36,
                              minHeight: 36,
                            ),
                            onPressed: _toggleSearch,
                          ),
                        ],
                      ),
                    )
                  else
                    IconButton(
                      icon: Icon(
                        Icons.search,
                        color: AppTheme.appBarColor,
                        size: 20,
                      ),
                      padding: EdgeInsets.all(4),
                      constraints: BoxConstraints(minWidth: 36, minHeight: 36),
                      onPressed: _toggleSearch,
                    ),
                ],
              ),
            ),
            content: Container(
              width: double.maxFinite,
              height: MediaQuery.of(context).size.height * 0.6,
              child: Column(
                children: [
                  if (!_isSearching) SizedBox(height: 8),
                  Row(
                    children: [
                      Text(
                        '${filteredContacts.length} contactos encontrados',
                        style: TextStyle(
                          color: AppTheme.textSecondary,
                          fontSize: 12,
                        ),
                      ),
                    ],
                  ),
                  SizedBox(height: 12),

                  Expanded(
                    child: filteredContacts.isEmpty
                        ? Center(
                            child: Column(
                              mainAxisAlignment: MainAxisAlignment.center,
                              children: [
                                Icon(
                                  _isSearching &&
                                          searchController.text.isNotEmpty
                                      ? Icons.search_off
                                      : Icons.contacts_outlined,
                                  size: 60,
                                  color: AppTheme.textLight,
                                ),
                                SizedBox(height: 16),
                                Text(
                                  _isSearching &&
                                          searchController.text.isNotEmpty
                                      ? 'Nenhum resultado para "${searchController.text}"'
                                      : 'Nenhum contacto encontrado',
                                  style: TextStyle(
                                    color: AppTheme.textSecondary,
                                  ),
                                  textAlign: TextAlign.center,
                                ),
                              ],
                            ),
                          )
                        : ListView.builder(
                            itemCount: filteredContacts.length,
                            itemBuilder: (context, index) {
                              final contact = filteredContacts[index];
                              return _buildContactItem(contact);
                            },
                          ),
                  ),
                ],
              ),
            ),
            actions: [
              TextButton(
                onPressed: () => Navigator.pop(context),
                child: Text('Voltar'),
              ),
            ],
          );
        },
      ),
    );
  }

  // ✅ ITEM DA LISTA DE CONTATOS
  Widget _buildContactItem(Contact contact) {
    final displayName = contact.displayName.isEmpty
        ? 'Sem nome'
        : contact.displayName;
    final phones = contact.phones.isNotEmpty
        ? contact.phones.first.number
        : 'Sem telefone';

    return Container(
      decoration: BoxDecoration(
        border: Border(bottom: BorderSide(color: AppTheme.dividerColor)),
      ),
      child: ListTile(
        leading: contact.photo != null
            ? CircleAvatar(
                radius: 22,
                backgroundImage: MemoryImage(contact.photo!),
              )
            : CircleAvatar(
                radius: 22,
                backgroundColor: AppTheme.avatarBackground,
                child: Icon(Icons.person, color: AppTheme.avatarIcon, size: 24),
              ),
        title: Text(
          displayName,
          style: TextStyle(fontWeight: FontWeight.w500, fontSize: 16),
        ),
        subtitle: Text(
          phones,
          style: TextStyle(color: AppTheme.textSecondary, fontSize: 14),
        ),
        trailing: Icon(Icons.chat, color: AppTheme.appBarColor),
        onTap: () {
          Navigator.pop(context);
          _startNewChat(contact);
        },
      ),
    );
  }

  // ✅ INICIAR NOVO CHAT
  Future<void> _startNewChat(Contact contact) async {
    setState(() => _isLoading = true);
    try {
      final remoteUserId = await ContactsHelper.fetchBackendUserId(contact);
      if (!mounted) return;

      if (remoteUserId == null) {
        showDialog(
          context: context,
          builder: (context) => AlertDialog(
            title: Text('Contato não registado'),
            content: Text(
              '${contact.displayName} ainda não usa o SpeekJoy. Quer convidá-lo?',
            ),
            actions: [
              TextButton(
                onPressed: () => Navigator.pop(context),
                child: Text('Cancelar', style: TextStyle(color: Colors.grey)),
              ),
              TextButton(
                onPressed: () {
                  Navigator.pop(context);
                  _sendInvite(contact);
                },
                child: Text(
                  'Convidar',
                  style: TextStyle(color: AppTheme.appBarColor),
                ),
              ),
            ],
          ),
        );
        setState(() => _isLoading = false);
        return;
      }

      Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => ChatPage(
            contact: ChatContact(
              contactId: remoteUserId,
              name: contact.displayName.isEmpty
                  ? 'Sem nome'
                  : contact.displayName,
              phoneNumber: contact.phones.isNotEmpty
                  ? contact.phones.first.number
                  : null,
              lastMessage: '',
              lastMessageTime: DateTime.now(),
              unreadCount: 0,
            ),
            remoteUserId: remoteUserId,
          ),
        ),
      );
    } catch (e) {
      if (!mounted) return;
      ScaffoldMessenger.of(
        context,
      ).showSnackBar(SnackBar(content: Text('Erro ao iniciar conversa: $e')));
    } finally {
      if (mounted) setState(() => _isLoading = false);
    }
  }

  // ✅ RESTANTE DAS FUNÇÕES (mantidas iguais)
  void _showPermissionDeniedDialog() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Permissão Necessária'),
        content: Text(
          'Para selecionar contatos, é necessário permitir o acesso à lista de contatos.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Cancelar'),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              openAppSettings();
            },
            child: Text('Configurações'),
            style: ElevatedButton.styleFrom(
              backgroundColor: AppTheme.appBarColor,
            ),
          ),
        ],
      ),
    );
  }

  Future<void> _logout() async {
    setState(() => _isLoading = true);
    try {
      await AuthService.logout();
      Navigator.pushNamedAndRemoveUntil(context, '/welcome', (route) => false);
    } catch (e) {
      print('❌ Erro no logout: $e');
    } finally {
      setState(() => _isLoading = false);
    }
  }

  // ✅ ESCUTAR QUEM ESTÁ DIGITANDO PARA MOSTRAR NA LISTA
  void _setupTypingListener() {
    _typingSubscription = ChatService.typingStream.listen((data) {
      final fromId = data['from']?.toString();
      final isTyping = data['is_typing'] == true;

      if (fromId != null && mounted) {
        setState(() {
          _typingUsers[fromId] = isTyping;
        });

        // Segurança: limpar status após 6 segundos se não vier o 'stop'
        _typingTimers[fromId]?.cancel();
        if (isTyping) {
          _typingTimers[fromId] = Timer(Duration(seconds: 6), () {
            if (mounted) {
              setState(() {
                _typingUsers[fromId] = false;
              });
            }
          });
        }
      }
    });
  }

  Future<void> _revokeOtherSessions() async {
    setState(() => _isLoading = true);
    try {
      final success = await AuthService.revokeOtherSessions();
      if (success) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('✅ Outras sessões foram revogadas com sucesso!'),
            backgroundColor: AppTheme.appBarColor,
          ),
        );
      } else {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('❌ Erro ao revogar outras sessões'),
            backgroundColor: Colors.red,
          ),
        );
      }
    } catch (e) {
      ScaffoldMessenger.of(
        context,
      ).showSnackBar(SnackBar(content: Text('Erro: $e')));
    } finally {
      setState(() => _isLoading = false);
    }
  }

  // ✅ DIÁLOGO DE CONFIRMAÇÃO PARA EXCLUIR CONVERSA
  void _showDeleteChatDialog(ChatContact chat) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Eliminar esta conversa?'),
        content: Text(
          'A conversa com "${chat.name}" será removida desta lista.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text(
              'Cancelar',
              style: TextStyle(color: AppTheme.primaryGreen),
            ),
          ),
          TextButton(
            onPressed: () {
              Navigator.pop(context);
              _deleteChat(chat);
            },
            child: Text(
              'Eliminar conversa',
              style: TextStyle(color: AppTheme.primaryGreen),
            ),
          ),
        ],
      ),
    );
  }

  void _deleteChat(ChatContact chat) async {
    await ChatService.deleteChat(chat.contactId);
    _clearSelection(); // Limpar seleção após deletar
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text('Conversa eliminada'),
        duration: Duration(seconds: 2),
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    return PopScope(
      // ✅ Tratar botão de voltar: permitir minimizar app quando não há mais telas
      canPop: true, // ✅ Permitir pop normalmente
      onPopInvoked: (didPop) {
        if (didPop) {
          // ✅ Verificar DEPOIS do pop se voltou para StartupPage (tela branca)
          WidgetsBinding.instance.addPostFrameCallback((_) {
            if (mounted) {
              final route = ModalRoute.of(context);
              if (route?.settings.name == '/') {
                // ✅ Voltou para StartupPage (tela branca) - redirecionar para welcome
                Navigator.pushReplacementNamed(context, '/welcome');
              }
            }
          });
        }
      },
      child: Scaffold(
        backgroundColor: AppTheme.surfaceColor,
        appBar: _isSelectionMode ? _buildSelectionAppBar() : _buildNormalAppBar(),
      body: Column(
        children: [
          Container(
            color: AppTheme.surfaceColor,
            padding: EdgeInsets.all(16),
            child: Container(
              decoration: BoxDecoration(
                color: AppTheme.searchBackground,
                borderRadius: BorderRadius.circular(20),
              ),
              child: TextField(
                controller: _searchController,
                onChanged: (value) => setState(() => _searchQuery = value),
                decoration: InputDecoration(
                  hintText: 'Pesquisar ou começar nova conversa',
                  hintStyle: TextStyle(color: AppTheme.textSecondary),
                  prefixIcon: Icon(Icons.search, color: AppTheme.textSecondary),
                  border: InputBorder.none,
                  contentPadding: EdgeInsets.symmetric(
                    horizontal: 16,
                    vertical: 12,
                  ),
                ),
              ),
            ),
          ),
          Expanded(
            child: TabBarView(
              controller: _tabController,
              children: [_buildChatsTab(), _buildStatusTab(), _buildCallsTab()],
            ),
          ),
        ],
      ),
      floatingActionButton: _isSelectionMode
          ? null // Ocultar botão "Nova Conversa" durante seleção
          : FloatingActionButton(
              onPressed: _openContactsList,
              backgroundColor: AppTheme.appBarColor,
              child: _isLoading
                  ? SizedBox(
                      width: 20,
                      height: 20,
                      child: CircularProgressIndicator(
                        strokeWidth: 2,
                        valueColor: AlwaysStoppedAnimation<Color>(
                          AppTheme.textOnGreen,
                        ),
                      ),
                    )
                  : Icon(Icons.chat, color: AppTheme.textOnGreen),
            ),
      ),
    );
  }

  // CHATS TAB COM DADOS REAIS
  Widget _buildChatsTab() {
    final filteredChats = _getFilteredChats();

    if (filteredChats.isEmpty) {
      return _buildEmptyChatsState();
    }

    return ListView.builder(
      padding: EdgeInsets.only(top: 8),
      itemCount: filteredChats.length,
      itemBuilder: (context, index) => _buildRealChatItem(filteredChats[index]),
    );
  }

  // ITEM DE CHAT REAL atualizado
  Widget _buildRealChatItem(ChatContact chat) {
    final isSelected = _selectedChatIds.contains(chat.contactId);

    return Container(
      // ✅ Fundo de seleção usando a cor da AppBar com opacidade
      color: isSelected ? AppTheme.appBarColor.withOpacity(0.15) : null,
      child: Container(
        decoration: BoxDecoration(
          border: Border(
            bottom: BorderSide(color: AppTheme.dividerColor, width: 0.5),
          ),
        ),
        child: ListTile(
          leading: Stack(
            children: [
              CircleAvatar(
                radius: 25,
                backgroundImage: chat.photo != null
                    ? MemoryImage(chat.photo!)
                    : null,
                child: chat.photo == null
                    ? Icon(
                        Icons.person,
                        color: AppTheme.textSecondary,
                        size: 28,
                      )
                    : null,
                backgroundColor: chat.photo == null
                    ? AppTheme.avatarBackground
                    : null,
              ),
              if (isSelected)
                Positioned(
                  bottom: -2,
                  right: -2,
                  child: Container(
                    decoration: BoxDecoration(
                      // ✅ Checkmark usando a cor exata da AppBar
                      color: AppTheme.appBarColor,
                      shape: BoxShape.circle,
                      border: Border.all(
                        color: AppTheme.surfaceColor,
                        width: 2,
                      ),
                    ),
                    padding: EdgeInsets.all(2),
                    child: Icon(Icons.check, size: 14, color: Colors.white),
                  ),
                ),
            ],
          ),
          title: Text(
            chat.name,
            style: TextStyle(fontWeight: FontWeight.w500, fontSize: 16),
          ),
          onLongPress: () => _toggleSelection(chat.contactId),
          onTap: () {
            if (_isSelectionMode) {
              _toggleSelection(chat.contactId);
            } else {
              _startNewChatFromItem(chat);
            }
          },
          subtitle: Row(
            children: [
              if (_typingUsers[chat.contactId] == true)
                Expanded(
                  child: Text(
                    'a escrever...',
                    style: TextStyle(
                      color: AppTheme.appBarColor,
                      fontSize: 14,
                      fontWeight: FontWeight.w500,
                    ),
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                  ),
                )
              else ...[
                if (chat.lastMessageEdited)
                  Icon(Icons.edit, size: 12, color: AppTheme.actionEdit),
                if (chat.lastMessageEdited) SizedBox(width: 4),
                Expanded(
                  child: Text(
                    chat.lastMessage,
                    style: TextStyle(
                      color: AppTheme.textSecondary,
                      fontSize: 14,
                    ),
                    maxLines: 1,
                    overflow: TextOverflow.ellipsis,
                  ),
                ),
              ],
            ],
          ),
          trailing: Column(
            mainAxisAlignment: MainAxisAlignment.center,
            crossAxisAlignment: CrossAxisAlignment.end,
            children: [
              Text(
                _formatTime(chat.lastMessageTime),
                style: TextStyle(color: AppTheme.textLight, fontSize: 12),
              ),
              if (chat.unreadCount > 0)
                Container(
                  margin: EdgeInsets.only(top: 4),
                  padding: EdgeInsets.symmetric(horizontal: 6, vertical: 2),
                  decoration: BoxDecoration(
                    color: AppTheme.appBarColor,
                    borderRadius: BorderRadius.circular(10),
                  ),
                  child: Text(
                    '${chat.unreadCount}',
                    style: TextStyle(color: Colors.white, fontSize: 10),
                  ),
                ),
            ],
          ),
        ),
      ),
    );
  }

  // FILTRAR CHATS POR PESQUISA
  List<ChatContact> _getFilteredChats() {
    if (_searchQuery.isEmpty) return _chats;

    return _chats.where((chat) {
      return chat.name.toLowerCase().contains(_searchQuery.toLowerCase()) ||
          chat.lastMessage.toLowerCase().contains(_searchQuery.toLowerCase());
    }).toList();
  }

  // ESTADO VAZIO PARA CHATS
  Widget _buildEmptyChatsState() {
    // ✅ Se estiver carregando, mostrar nada (tela limpa) para sensação de rapidez
    if (_isLoading) {
      return const SizedBox.shrink();
    }

    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Icon(Icons.chat_bubble_outline, size: 80, color: AppTheme.textLight),
          SizedBox(height: 16),
          Text(
            'Nenhuma conversa',
            style: TextStyle(color: AppTheme.textSecondary, fontSize: 18),
          ),
          SizedBox(height: 8),
          Text(
            'Inicie uma conversa para ver os chats aqui',
            style: TextStyle(color: AppTheme.textSecondary, fontSize: 14),
          ),
          SizedBox(height: 16),
          ElevatedButton.icon(
            onPressed: _openContactsList,
            icon: Icon(Icons.chat),
            label: Text('Iniciar primeira conversa'),
            style: ElevatedButton.styleFrom(
              backgroundColor: AppTheme.appBarColor,
              foregroundColor: AppTheme.textOnGreen,
            ),
          ),
        ],
      ),
    );
  }

  // FORMATAR HORA
  String _formatTime(DateTime timestamp) {
    final now = DateTime.now();
    final today = DateTime(now.year, now.month, now.day);
    final messageDay = DateTime(timestamp.year, timestamp.month, timestamp.day);

    if (messageDay == today) {
      return '${timestamp.hour.toString().padLeft(2, '0')}:${timestamp.minute.toString().padLeft(2, '0')}';
    } else {
      return '${timestamp.day}/${timestamp.month}';
    }
  }

  // ✅ TABS DE STATUS E CALLS
  Widget _buildStatusTab() {
    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Icon(Icons.radio_button_checked, size: 80, color: AppTheme.textLight),
          SizedBox(height: 20),
          Text(
            'Status em breve',
            style: TextStyle(fontSize: 18, color: AppTheme.textSecondary),
          ),
          SizedBox(height: 10),
          Text(
            'Esta funcionalidade será implementada em breve',
            style: TextStyle(fontSize: 14, color: AppTheme.textLight),
            textAlign: TextAlign.center,
          ),
        ],
      ),
    );
  }

  Widget _buildCallsTab() {
    return Center(
      child: Column(
        mainAxisAlignment: MainAxisAlignment.center,
        children: [
          Icon(Icons.call, size: 80, color: AppTheme.textLight),
          SizedBox(height: 20),
          Text(
            'Chamadas em breve',
            style: TextStyle(fontSize: 18, color: AppTheme.textSecondary),
          ),
          SizedBox(height: 10),
          Text(
            'Esta funcionalidade será implementada em breve',
            style: TextStyle(fontSize: 14, color: AppTheme.textLight),
            textAlign: TextAlign.center,
          ),
        ],
      ),
    );
  }

  void _showRevokeDialog() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Sair de outros dispositivos'),
        content: Text(
          'Isso irá desconectar todos os outros dispositivos que estão usando sua conta. Você permanecerá conectado neste dispositivo. Deseja continuar?',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Cancelar'),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              _revokeOtherSessions();
            },
            child: Text('Confirmar'),
            style: ElevatedButton.styleFrom(
              backgroundColor: AppTheme.actionDelete,
              foregroundColor: AppTheme.textOnGreen,
            ),
          ),
        ],
      ),
    );
  }

  // Novo método auxiliar para abrir chat do item
  void _startNewChatFromItem(ChatContact chat) {
    print('👆 Clicado no chat: ${chat.name} (Unread: ${chat.unreadCount})');

    // ✅ RESTAURADO: MARCAR COMO LIDO ANTES DE ABRIR
    ChatService.markChatAsRead(chat.contactId);

    Navigator.push(
      context,
      MaterialPageRoute(
        builder: (context) =>
            ChatPage(contact: chat, remoteUserId: chat.contactId),
      ),
    );
  }

  // ✅ ENVIAR CONVITE VIA SMS
  Future<void> _sendInvite(Contact contact) async {
    if (contact.phones.isEmpty) return;
    final phone = contact.phones.first.number;

    // Preparar URI para SMS
    final Uri smsLaunchUri = Uri(
      scheme: 'sms',
      path: phone,
      queryParameters: <String, String>{
        'body':
            'Olá! Venha conversar comigo no SpeekJoy! Baixe o app aqui: [LINK]',
      },
    );

    try {
      if (await canLaunchUrl(smsLaunchUri)) {
        await launchUrl(smsLaunchUri);
      } else {
        // Fallback para alguns dispositivos Android
        await launchUrl(smsLaunchUri, mode: LaunchMode.externalApplication);
      }
    } catch (e) {
      if (!mounted) return;
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Não foi possível abrir o app de SMS.')),
      );
    }
  }
}
