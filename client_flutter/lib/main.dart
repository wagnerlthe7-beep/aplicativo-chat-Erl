import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'startup_page.dart';
import 'welcome_page.dart';
import 'phone_input_page.dart';
import 'otp_page.dart';
import 'name_input_page.dart';
import 'permissions_page.dart';
import 'chat_list_page.dart';

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

class MyApp extends StatelessWidget {
  const MyApp({super.key});

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
