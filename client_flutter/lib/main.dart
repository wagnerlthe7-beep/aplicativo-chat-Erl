import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'startup_page.dart';
import 'welcome_page.dart';
import 'phone_input_page.dart';
import 'otp_page.dart';
import 'name_input_page.dart';
import 'permissions_page.dart';
import 'chat_list_page.dart';
import 'chat_service.dart'; // Import necessário
import 'websocket_foreground_service.dart'; // Foreground service
import 'notification_service.dart'; // Serviço de notificações

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  print('🚀 Iniciando aplicação WhaClone...');

  try {
    await Firebase.initializeApp();
    print('✅ Firebase inicializado');

    // ✅ INICIALIZAR FOREGROUND SERVICE
    await WebSocketForegroundService.initialize();
    print('✅ Foreground Service inicializado');

    // ✅ INICIALIZAR SERVIÇO DE NOTIFICAÇÕES
    await NotificationService().initialize();
    await NotificationService().requestPermission();
    print('✅ Serviço de notificações inicializado');

    // ✅ INICIALIZAR SISTEMA DE CHATS
    //await ChatService.initializeChatList();
    //print('✅ Sistema de chats inicializado');
  } catch (e) {
    print('❌ Erro na inicialização: $e');
  }

  runApp(const MyApp());
}

class MyApp extends StatefulWidget {
  const MyApp({super.key});

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
      theme: ThemeData(
        primaryColor: Colors.green,
        primarySwatch: Colors.green,
        visualDensity: VisualDensity.adaptivePlatformDensity,
      ),
      initialRoute: '/',
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
