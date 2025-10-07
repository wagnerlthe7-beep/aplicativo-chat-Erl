import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'startup_page.dart';
import 'welcome_page.dart';
import 'phone_input_page.dart';
import 'otp_page.dart';
import 'chat_list_page.dart';

void main() async {
  WidgetsFlutterBinding.ensureInitialized();
  await Firebase.initializeApp();
  runApp(const MyApp());
}

class MyApp extends StatelessWidget {
  const MyApp({super.key});

  // This widget is the root of your application.
  @override
  Widget build(BuildContext context) {
    return MaterialApp(
      title: 'WhaClone',
      debugShowCheckedModeBanner: false,
      initialRoute: '/',
      routes: {
        '/': (c) => StartupPage(),
        '/welcome': (c) => WelcomePage(),
        '/phone': (c) => PhoneInputPage(),
        '/otp': (c) => OtpPage(),
        '/chatList': (c) => ChatListPage(),
      },
    );
  }
}
