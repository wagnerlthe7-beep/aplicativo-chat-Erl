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

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  print('🚀 Iniciando aplicação WhaClone...');

  try {
    await Firebase.initializeApp();
    print('✅ Firebase inicializado');

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
  @override
  void initState() {
    super.initState();
    // ✅ Registrar observador de ciclo de vida
    WidgetsBinding.instance.addObserver(this);
  }

  @override
  void dispose() {
    // ✅ Remover observador
    WidgetsBinding.instance.removeObserver(this);
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    print('🔄 App Lifecycle State changed to: $state');

    if (state == AppLifecycleState.paused) {
      // 🌑 App em background:
      // - Enviar presença "offline"
      // - Desconectar WebSocket para economizar bateria e evitar conflitos
      print('🌑 App em Background -> Enviando presença offline e desconectando');
      ChatService.sendPresence('offline');
      ChatService.disconnect();
    } else if (state == AppLifecycleState.resumed) {
      // ☀️ App em foreground:
      // - Reconectar WebSocket
      // - Enviar presença "online"
      print('☀️ App em Foreground -> Reconectando e enviando presença online');
      // Primeiro conectar (se necessário), depois enviar presença
      ChatService.connect().then((_) {
         ChatService.sendPresence('online');
      });
    }
  }

  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'WhaClone',
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
