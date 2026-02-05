// contact_search_page.dart
// Página de pesquisa de contatos OFFLINE-FIRST
// Abre imediatamente, mostra contatos locais, sincroniza em background quando online

import 'package:flutter/material.dart';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:permission_handler/permission_handler.dart';
import 'package:url_launcher/url_launcher.dart';
import 'package:connectivity_plus/connectivity_plus.dart';
import 'package:http/http.dart' as http;
import 'dart:async';
import 'dart:convert';
import 'chat_page.dart';
import 'chat_model.dart';
import 'contacts_helper.dart';
import 'auth_service.dart';
import 'services/contacts_cache_service.dart';
import 'app_theme.dart';

class ContactSearchPage extends StatefulWidget {
  @override
  _ContactSearchPageState createState() => _ContactSearchPageState();
}

class _ContactSearchPageState extends State<ContactSearchPage> {
  final TextEditingController _searchController = TextEditingController();
  List<Contact> _allContacts = [];
  List<Contact> _filteredContacts = [];
  bool _isLoading = false;
  bool _isSyncing = false;
  bool _isOnline = true;
  String? _syncError;
  bool _isSearching = false; // Controla se o campo de pesquisa está visível
  StreamSubscription<List<ConnectivityResult>>? _connectivitySubscription;

  // Cache de status de registro (phone -> {userId, isRegistered})
  final Map<String, Map<String, dynamic>> _registrationCache = {};

  @override
  void initState() {
    super.initState();
    _loadLocalContacts();
    _checkConnectivity();
    _setupConnectivityListener();
  }

  @override
  void dispose() {
    _searchController.dispose();
    _connectivitySubscription?.cancel();
    super.dispose();
  }

  /// Verificar conectividade
  Future<void> _checkConnectivity() async {
    final connectivityResult = await Connectivity().checkConnectivity();
    setState(() {
      _isOnline =
          connectivityResult.contains(ConnectivityResult.mobile) ||
          connectivityResult.contains(ConnectivityResult.wifi);
    });

    // Se estiver online, sincronizar em background
    if (_isOnline) {
      _syncContactsWithServer();
    }
  }

  /// Escutar mudanças de conectividade
  void _setupConnectivityListener() {
    _connectivitySubscription = Connectivity().onConnectivityChanged.listen((
      List<ConnectivityResult> results,
    ) {
      final wasOffline = !_isOnline;
      setState(() {
        _isOnline =
            results.contains(ConnectivityResult.mobile) ||
            results.contains(ConnectivityResult.wifi);
      });

      // Se voltou online, sincronizar
      if (wasOffline && _isOnline) {
        _syncContactsWithServer();
      }
    });
  }

  /// ✅ PASSO 1: Carregar contatos LOCAIS (instantâneo, sem servidor)
  Future<void> _loadLocalContacts() async {
    setState(() => _isLoading = true);

    try {
      // Verificar permissão
      final permission = await Permission.contacts.status;
      if (!permission.isGranted) {
        final result = await Permission.contacts.request();
        if (!result.isGranted) {
          if (mounted) {
            _showPermissionDeniedDialog();
            setState(() => _isLoading = false);
          }
          return;
        }
      }

      // ✅ Carregar contatos locais (RÁPIDO, não depende do servidor)
      final contacts = await FlutterContacts.getContacts(
        withProperties: true,
        withPhoto: true,
      );

      if (mounted) {
        setState(() {
          _allContacts = contacts;
          _filteredContacts = contacts;
          _isLoading = false;
        });

        // ✅ Carregar cache de registros do SQLite
        await _loadRegistrationCache();

        // ✅ Se estiver online, sincronizar em background (não bloqueia UI)
        if (_isOnline) {
          _syncContactsWithServer();
        }
      }
    } catch (e) {
      print('❌ Erro ao carregar contatos: $e');
      if (mounted) {
        setState(() {
          _isLoading = false;
          _syncError = 'Erro ao carregar contatos: $e';
        });
      }
    }
  }

  /// ✅ PASSO 2: Carregar cache de registros do SQLite
  Future<void> _loadRegistrationCache() async {
    for (final contact in _allContacts) {
      if (contact.phones.isEmpty) continue;

      final phone = _normalizePhone(contact.phones.first.number);
      final cached = await ContactsCacheService.getContact(phone);

      if (cached != null) {
        _registrationCache[phone] = {
          'user_id': cached['user_id'],
          'is_registered': cached['is_registered'] as bool,
        };
      }
    }

    if (mounted) {
      setState(() {}); // Atualizar UI com badges
    }
  }

  /// ✅ PASSO 3: Sincronizar com servidor em BACKGROUND (quando online)
  Future<void> _syncContactsWithServer() async {
    if (!_isOnline || _isSyncing) return;

    setState(() => _isSyncing = true);
    _syncError = null;

    try {
      // Buscar todos os telefones dos contatos
      final phones = _allContacts
          .where((c) => c.phones.isNotEmpty)
          .map((c) => _normalizePhone(c.phones.first.number))
          .toList();

      if (phones.isEmpty) {
        setState(() => _isSyncing = false);
        return;
      }

      // ✅ Enviar para servidor em batch
      final response = await _syncContactsBatch(phones);

      if (response != null) {
        // ✅ Salvar no cache SQLite
        await ContactsCacheService.saveContacts(response);

        // ✅ Atualizar cache em memória
        for (final contact in _allContacts) {
          if (contact.phones.isEmpty) continue;
          final phone = _normalizePhone(contact.phones.first.number);
          final cached = await ContactsCacheService.getContact(phone);
          if (cached != null) {
            _registrationCache[phone] = {
              'user_id': cached['user_id'],
              'is_registered': cached['is_registered'] as bool,
            };
          }
        }

        if (mounted) {
          setState(() {}); // Atualizar UI
        }
      }
    } catch (e) {
      print('❌ Erro ao sincronizar contatos: $e');
      if (mounted) {
        setState(() {
          _syncError = 'Erro ao sincronizar: $e';
        });
      }
    } finally {
      if (mounted) {
        setState(() => _isSyncing = false);
      }
    }
  }

  /// Sincronizar batch de contatos com servidor
  Future<List<Map<String, dynamic>>?> _syncContactsBatch(
    List<String> phones,
  ) async {
    try {
      final accessToken = await AuthService.getAccessToken();
      if (accessToken == null) return null;

      final url = Uri.parse('${AuthService.backendUrl}/users/lookup');
      final response = await http.post(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: jsonEncode({'phones': phones}),
      );

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        final users = data['users'] as List<dynamic>? ?? [];

        // Mapear resultado para cache
        // NOTA: O servidor retorna users com phone, mas precisamos mapear pelo phone normalizado
        final Map<String, Map<String, dynamic>> phoneToUser = {};
        for (final user in users) {
          final userPhone = user['phone']?.toString();
          if (userPhone != null) {
            final normalizedPhone = _normalizePhone(userPhone);
            phoneToUser[normalizedPhone] = {
              'phone': normalizedPhone,
              'user_id': user['id']?.toString(),
              'name': user['name']?.toString(),
              'is_registered': true,
            };
          }
        }

        // Criar lista completa (incluindo não registrados)
        final result = phones.map((phone) {
          if (phoneToUser.containsKey(phone)) {
            return phoneToUser[phone]!;
          } else {
            return {
              'phone': phone,
              'user_id': null,
              'name': null,
              'is_registered': false,
            };
          }
        }).toList();

        return result;
      }
    } catch (e) {
      print('❌ Erro no sync batch: $e');
    }
    return null;
  }

  /// Normalizar telefone (remover espaços, traços, etc)
  String _normalizePhone(String phone) {
    return phone.replaceAll(RegExp(r'[\s\-\(\)]'), '');
  }

  /// Filtrar contatos por pesquisa
  void _filterContacts(String query) {
    if (query.isEmpty) {
      setState(() => _filteredContacts = _allContacts);
      return;
    }

    final queryLower = query.toLowerCase();
    setState(() {
      _filteredContacts = _allContacts.where((contact) {
        final name = contact.displayName.toLowerCase();
        final phones = contact.phones
            .map((phone) => phone.number)
            .join(' ')
            .toLowerCase();

        return name.contains(queryLower) ||
            phones.contains(queryLower) ||
            phones.replaceAll(RegExp(r'[\s\-\(\)]'), '').contains(queryLower);
      }).toList();
    });
  }

  /// Verificar se contato está registrado (com cache)
  bool _isContactRegistered(Contact contact) {
    if (contact.phones.isEmpty) return false;
    final phone = _normalizePhone(contact.phones.first.number);
    return _registrationCache[phone]?['is_registered'] == true;
  }

  /// Obter userId do contato (com cache)
  String? _getContactUserId(Contact contact) {
    if (contact.phones.isEmpty) return null;
    final phone = _normalizePhone(contact.phones.first.number);
    return _registrationCache[phone]?['user_id'] as String?;
  }

  /// Iniciar chat com contato
  Future<void> _startChat(Contact contact) async {
    // ✅ Verificar cache primeiro (OFFLINE-FIRST)
    final userId = _getContactUserId(contact);
    final isRegistered = _isContactRegistered(contact);

    if (userId != null && isRegistered) {
      // ✅ Contato registrado - abrir chat imediatamente
      Navigator.push(
        context,
        MaterialPageRoute(
          builder: (context) => ChatPage(
            contact: ChatContact(
              contactId: userId,
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
            remoteUserId: userId,
          ),
        ),
      );
      return;
    }

    // Se não está no cache ou não está registrado, verificar servidor (se online)
    if (_isOnline) {
      setState(() => _isLoading = true);
      try {
        final remoteUserId = await ContactsHelper.fetchBackendUserId(contact);
        setState(() => _isLoading = false);

        if (remoteUserId != null) {
          // ✅ Salvar no cache
          final phone = _normalizePhone(contact.phones.first.number);
          await ContactsCacheService.saveContact(
            phone: phone,
            userId: remoteUserId,
            name: contact.displayName,
            isRegistered: true,
          );

          // Atualizar cache em memória
          _registrationCache[phone] = {
            'user_id': remoteUserId,
            'is_registered': true,
          };

          // Abrir chat
          if (mounted) {
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
          }
        } else {
          // Não registrado - mostrar diálogo de convite
          _showInviteDialog(contact);
        }
      } catch (e) {
        setState(() => _isLoading = false);
        if (mounted) {
          ScaffoldMessenger.of(context).showSnackBar(
            SnackBar(content: Text('Erro ao verificar contato: $e')),
          );
        }
      }
    } else {
      // Offline e não está no cache - mostrar diálogo
      _showInviteDialog(contact);
    }
  }

  /// Mostrar diálogo de convite
  void _showInviteDialog(Contact contact) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Contato não registrado'),
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
  }

  /// Enviar convite via SMS
  Future<void> _sendInvite(Contact contact) async {
    if (contact.phones.isEmpty) return;
    final phone = contact.phones.first.number;

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
        await launchUrl(smsLaunchUri, mode: LaunchMode.externalApplication);
      }
    } catch (e) {
      if (mounted) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Não foi possível abrir o app de SMS.')),
        );
      }
    }
  }

  /// Botão de ação (Novo grupo, Novo contacto, etc)
  Widget _buildActionButton({
    required IconData icon,
    required String label,
    required VoidCallback onTap,
    bool showQrCode = false,
  }) {
    return GestureDetector(
      onTap: onTap,
      child: Column(
        mainAxisSize: MainAxisSize.min,
        children: [
          Stack(
            alignment: Alignment.center,
            children: [
              Container(
                width: 56,
                height: 56,
                decoration: BoxDecoration(
                  color: AppTheme.appBarColor,
                  shape: BoxShape.circle,
                ),
                child: Icon(icon, color: Colors.white, size: 28),
              ),
              if (showQrCode)
                Positioned(
                  bottom: 0,
                  right: 0,
                  child: Container(
                    width: 20,
                    height: 20,
                    decoration: BoxDecoration(
                      color: Colors.white,
                      shape: BoxShape.circle,
                      border: Border.all(color: AppTheme.appBarColor, width: 2),
                    ),
                    child: Icon(
                      Icons.qr_code,
                      size: 12,
                      color: AppTheme.appBarColor,
                    ),
                  ),
                ),
            ],
          ),
          SizedBox(height: 8),
          Text(
            label,
            style: TextStyle(fontSize: 12, color: AppTheme.textSecondary),
            textAlign: TextAlign.center,
          ),
        ],
      ),
    );
  }

  /// Mostrar diálogo de permissão negada
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

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: AppTheme.surfaceColor,
      appBar: AppBar(
        backgroundColor: AppTheme.appBarColor,
        elevation: 0,
        leading: IconButton(
          icon: Icon(Icons.arrow_back, color: AppTheme.textOnGreen),
          onPressed: () => Navigator.pop(context),
        ),
        title: Column(
          crossAxisAlignment: CrossAxisAlignment.start,
          mainAxisSize: MainAxisSize.min,
          children: [
            Text(
              'Selecionar contacto',
              style: TextStyle(
                color: AppTheme.textOnGreen,
                fontSize: 18,
                fontWeight: FontWeight.w500,
              ),
            ),
            Text(
              '${_filteredContacts.length} contactos',
              style: TextStyle(
                color: AppTheme.textOnGreen.withOpacity(0.8),
                fontSize: 13,
              ),
            ),
          ],
        ),
        actions: [
          IconButton(
            icon: Icon(
              _isSearching ? Icons.close : Icons.search,
              color: AppTheme.textOnGreen,
            ),
            onPressed: () {
              setState(() {
                _isSearching = !_isSearching;
                if (!_isSearching) {
                  _searchController.clear();
                  _filterContacts('');
                }
              });
            },
          ),
          PopupMenuButton<String>(
            icon: Icon(Icons.more_vert, color: AppTheme.textOnGreen),
            onSelected: (value) {
              if (value == 'sync') {
                if (_isOnline) {
                  _syncContactsWithServer();
                }
              }
            },
            itemBuilder: (context) => [
              PopupMenuItem(
                value: 'sync',
                enabled: _isOnline && !_isSyncing,
                child: Row(
                  children: [
                    Icon(
                      Icons.sync,
                      color: _isOnline && !_isSyncing
                          ? AppTheme.appBarColor
                          : Colors.grey,
                    ),
                    SizedBox(width: 8),
                    Text('Sincronizar contatos'),
                  ],
                ),
              ),
            ],
          ),
        ],
      ),
      body: Column(
        children: [
          // ✅ Campo de pesquisa (aparece quando _isSearching é true)
          if (_isSearching)
            Container(
              color: Colors.white,
              padding: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
              child: TextField(
                controller: _searchController,
                autofocus: true,
                onChanged: _filterContacts,
                decoration: InputDecoration(
                  hintText: 'Pesquisar contatos...',
                  prefixIcon: Icon(Icons.search, color: AppTheme.textSecondary),
                  suffixIcon: _searchController.text.isNotEmpty
                      ? IconButton(
                          icon: Icon(
                            Icons.clear,
                            color: AppTheme.textSecondary,
                          ),
                          onPressed: () {
                            _searchController.clear();
                            _filterContacts('');
                          },
                        )
                      : null,
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(20),
                    borderSide: BorderSide(color: AppTheme.dividerColor),
                  ),
                  filled: true,
                  fillColor: AppTheme.searchBackground,
                  contentPadding: EdgeInsets.symmetric(
                    horizontal: 16,
                    vertical: 12,
                  ),
                ),
              ),
            ),

          // ✅ Indicador de status (online/offline)
          if (!_isOnline || _syncError != null)
            Container(
              width: double.infinity,
              padding: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
              color: _isOnline ? Colors.orange.shade100 : Colors.grey.shade200,
              child: Row(
                children: [
                  Icon(
                    _isOnline ? Icons.warning : Icons.cloud_off,
                    size: 16,
                    color: _isOnline ? Colors.orange : Colors.grey,
                  ),
                  SizedBox(width: 8),
                  Expanded(
                    child: Text(
                      _syncError ??
                          'Sem internet - mostrando contatos do cache',
                      style: TextStyle(
                        fontSize: 12,
                        color: _isOnline
                            ? Colors.orange.shade900
                            : Colors.grey.shade700,
                      ),
                    ),
                  ),
                ],
              ),
            ),

          // ✅ Botões de ação (Novo grupo, Novo contacto)
          Container(
            color: Colors.white,
            padding: EdgeInsets.symmetric(vertical: 12, horizontal: 16),
            child: Row(
              mainAxisAlignment: MainAxisAlignment.spaceEvenly,
              children: [
                _buildActionButton(
                  icon: Icons.group_add,
                  label: 'Novo grupo',
                  onTap: () {
                    // TODO: Implementar novo grupo
                    ScaffoldMessenger.of(context).showSnackBar(
                      SnackBar(content: Text('Funcionalidade em breve')),
                    );
                  },
                ),
                _buildActionButton(
                  icon: Icons.person_add,
                  label: 'Novo contacto',
                  onTap: () {
                    // TODO: Implementar novo contacto
                    ScaffoldMessenger.of(context).showSnackBar(
                      SnackBar(content: Text('Funcionalidade em breve')),
                    );
                  },
                ),
              ],
            ),
          ),

          // ✅ Título da seção de contatos
          Container(
            width: double.infinity,
            padding: EdgeInsets.symmetric(horizontal: 16, vertical: 8),
            color: Colors.grey.shade100,
            child: Text(
              'Contactos no SpeekJoy',
              style: TextStyle(
                color: AppTheme.textSecondary,
                fontSize: 14,
                fontWeight: FontWeight.w500,
              ),
            ),
          ),

          // Lista de contatos
          Expanded(
            child: _isLoading
                ? Center(
                    child: CircularProgressIndicator(
                      valueColor: AlwaysStoppedAnimation<Color>(
                        AppTheme.appBarColor,
                      ),
                    ),
                  )
                : _filteredContacts.isEmpty
                ? Center(
                    child: Column(
                      mainAxisAlignment: MainAxisAlignment.center,
                      children: [
                        Icon(
                          _searchController.text.isNotEmpty
                              ? Icons.search_off
                              : Icons.contacts_outlined,
                          size: 60,
                          color: AppTheme.textLight,
                        ),
                        SizedBox(height: 16),
                        Text(
                          _searchController.text.isNotEmpty
                              ? 'Nenhum resultado para "${_searchController.text}"'
                              : 'Nenhum contacto encontrado',
                          style: TextStyle(color: AppTheme.textSecondary),
                          textAlign: TextAlign.center,
                        ),
                      ],
                    ),
                  )
                : ListView.builder(
                    itemCount: _filteredContacts.length,
                    itemBuilder: (context, index) {
                      final contact = _filteredContacts[index];
                      final isRegistered = _isContactRegistered(contact);

                      return Container(
                        decoration: BoxDecoration(
                          border: Border(
                            bottom: BorderSide(
                              color: AppTheme.dividerColor,
                              width: 0.5,
                            ),
                          ),
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
                                  child: Icon(
                                    Icons.person,
                                    color: AppTheme.avatarIcon,
                                    size: 24,
                                  ),
                                ),
                          title: Row(
                            children: [
                              Expanded(
                                child: Text(
                                  contact.displayName.isEmpty
                                      ? 'Sem nome'
                                      : contact.displayName,
                                  style: TextStyle(
                                    fontWeight: FontWeight.w500,
                                    fontSize: 16,
                                  ),
                                ),
                              ),
                              if (isRegistered)
                                Container(
                                  margin: EdgeInsets.only(left: 8),
                                  padding: EdgeInsets.symmetric(
                                    horizontal: 6,
                                    vertical: 2,
                                  ),
                                  decoration: BoxDecoration(
                                    color: AppTheme.appBarColor,
                                    borderRadius: BorderRadius.circular(8),
                                  ),
                                  child: Text(
                                    'SpeekJoy',
                                    style: TextStyle(
                                      color: Colors.white,
                                      fontSize: 10,
                                      fontWeight: FontWeight.bold,
                                    ),
                                  ),
                                ),
                            ],
                          ),
                          subtitle: Text(
                            contact.phones.isNotEmpty
                                ? contact.phones.first.number
                                : 'Sem telefone',
                            style: TextStyle(
                              color: AppTheme.textSecondary,
                              fontSize: 14,
                            ),
                          ),
                          trailing: Icon(
                            Icons.chat,
                            color: AppTheme.appBarColor,
                          ),
                          onTap: () => _startChat(contact),
                        ),
                      );
                    },
                  ),
          ),
        ],
      ),
    );
  }
}
