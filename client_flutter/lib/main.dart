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
      // - Enviar presença "offline" para que os outros vejam que saí
      // - MAS NÃO DESCONECTAR O WEBSOCKET! Assim recebo mensagens e envio Ack.
      print('🌑 App em Background -> Enviando presença offline (mantendo conexão)');
      ChatService.sendPresence('offline');
    } else if (state == AppLifecycleState.resumed) {
      // ☀️ App em foreground:
      // - Enviar presença "online"
      print('☀️ App em Foreground -> Enviando presença online');
      ChatService.sendPresence('online');
      
      // Opcional: Verificar conexão se caiu
      if (!ChatService.isConnected) {
         print('⚠️ WebSocket desconectado ao voltar. Tentando reconectar...');
         ChatService.connect();
      }
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
