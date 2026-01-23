import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'startup_page.dart';
import 'welcome_page.dart';
import 'phone_input_page.dart';
import 'otp_page.dart';
import 'name_input_page.dart';
import 'permissions_page.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart'; // ✅ Storage
import 'chat_list_page.dart';
import 'chat_service.dart'; // Import necessário
import 'websocket_foreground_service.dart'; // Foreground service
import 'notification_service.dart'; // Serviço de notificações
import 'services/message_sync_service.dart'; // ✅ Serviço de sincronização offline
import 'app_theme.dart';
import 'startup_page.dart'; // Ainda necessário para Fallback

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  print('🚀 Iniciando aplicação WhaClone...');

  String initialRoute = '/';

  try {
    await Firebase.initializeApp();
    print('✅ Firebase inicializado');

    // ✅ VERIFICAÇÃO DE SESSÃO RÁPIDA (Antes de renderizar)
    final storage = FlutterSecureStorage();
    final token = await storage.read(key: 'access_token');

    if (token != null) {
      print('🚀 Token encontrado! Pré-carregando chats...');
      initialRoute = '/chatList';
      
      // ✅ PRÉ-AQUECIMENTO: Carregar chats locais na memória AGORA
      await ChatService.loadLocalChats();
      // Não esperar conectar no main, apenas carregar o local
    } else {
      print('👋 Nenhum token, indo para WelcomePage');
      initialRoute = '/welcome';
    }

    // ✅ INICIALIZAR FOREGROUND SERVICE
    await WebSocketForegroundService.initialize();
    
    // ✅ INICIALIZAR SERVIÇO DE NOTIFICAÇÕES
    await NotificationService().initialize();
    await NotificationService().requestPermission();
    
    // ✅ INICIALIZAR SERVIÇO DE SINCRONIZAÇÃO OFFLINE-FIRST
    if (token != null) {
      await MessageSyncService.initialize();
      print('✅ MessageSyncService inicializado');
    }

  } catch (e) {
    print('❌ Erro na inicialização: $e');
  }

  runApp(MyApp(initialRoute: initialRoute));
}

class MyApp extends StatefulWidget {
  final String initialRoute; // ✅ Rota inicial dinâmica
  
  const MyApp({super.key, this.initialRoute = '/'});

  @override
  State<MyApp> createState() => _MyAppState();
}

class _MyAppState extends State<MyApp> with WidgetsBindingObserver {
  final AppLifecycleManager _lifecycleManager = AppLifecycleManager();

  @override
  void initState() {
    super.initState();
    // ✅ Registrar observador de ciclo de vida E gerenciador de foreground service
    WidgetsBinding.instance.addObserver(this);
    _lifecycleManager.initialize();
  }

  @override
  void dispose() {
    // ✅ Remover observador e limpar gerenciador
    WidgetsBinding.instance.removeObserver(this);
    _lifecycleManager.dispose();
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    print('🔄 App Lifecycle State changed to: $state');

    // Delegar para o AppLifecycleManager do foreground service
    // Ele vai cuidar de iniciar/parar o serviço automaticamente
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'SpeekJoy',
      debugShowCheckedModeBanner: false,
      theme: AppTheme.lightTheme,
      initialRoute: widget.initialRoute, // ✅ Usa a rota decidida no main()
      routes: {
        '/': (context) => StartupPage(),
        '/welcome': (context) => WelcomePage(),
        '/phone': (context) => PhoneInputPage(),
        '/otp': (context) => OtpPage(),
        '/nameInput': (context) => NameInputPage(phoneNumber: ''),
        '/permissions': (context) => PermissionsPage(),
        '/chatList': (context) => ChatListPage(),
      },
    );
  }
}
