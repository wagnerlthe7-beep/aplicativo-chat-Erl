import 'package:flutter/material.dart';
import 'package:firebase_core/firebase_core.dart';
import 'package:firebase_messaging/firebase_messaging.dart';
import 'startup_page.dart';
import 'welcome_page.dart';
import 'phone_input_page.dart';
import 'otp_page.dart';
import 'name_input_page.dart';
import 'permissions_page.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'chat_list_page.dart';
import 'chat_service.dart';
import 'notification_service.dart';
import 'fcm_service.dart';
import 'services/message_sync_service.dart';
import 'app_theme.dart';

/// Handler de background para FCM - DEVE ser top-level function
/// Esta função é executada em um isolate separado quando a app está em background
@pragma('vm:entry-point')
Future<void> _firebaseMessagingBackgroundHandler(RemoteMessage message) async {
  try {
    // ✅ IMPORTANTE: Logs em background podem não aparecer imediatamente
    // Usar print() que é mais confiável em background isolates
    print('🔔🔔🔔 [FCM Background Handler] INICIANDO PROCESSAMENTO 🔔🔔🔔');
    print(
      '   Notification: ${message.notification?.title} - ${message.notification?.body}',
    );
    print('   Data: ${message.data}');
    print('   MessageId: ${message.messageId}');
    print('   SentTime: ${message.sentTime}');
    print('   MessageType: ${message.messageType}');

    // Certificar que Firebase está inicializado
    // Em background isolates, Firebase pode não estar inicializado
    try {
      await Firebase.initializeApp();
      print('✅ Firebase inicializado no background handler');
    } catch (e) {
      // Firebase já pode estar inicializado
      print('⚠️ Firebase já inicializado ou erro: $e');
    }

    // Delegar para o FCMService
    await firebaseMessagingBackgroundHandler(message);
    print('✅ [FCM Background Handler] Processamento concluído');
  } catch (e, stackTrace) {
    print('❌❌❌ [FCM Background Handler] ERRO CRÍTICO: $e');
    print('   Stack trace: $stackTrace');
    // Re-throw para que o FCM saiba que houve erro
    rethrow;
  }
}

void main() async {
  WidgetsFlutterBinding.ensureInitialized();

  print('🚀 Iniciando aplicação SpeekJoy...');

  String initialRoute = '/';

  try {
    await Firebase.initializeApp();
    print('✅ Firebase inicializado');

    // ✅ REGISTRAR HANDLER DE BACKGROUND FCM (ANTES de qualquer outra coisa)
    // Isso permite que a app acorde e processe mensagens mesmo fechada
    FirebaseMessaging.onBackgroundMessage(_firebaseMessagingBackgroundHandler);
    print('✅ FCM Background Handler registrado');

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

    // ✅ INICIALIZAR SERVIÇO DE NOTIFICAÇÕES LOCAIS
    await NotificationService().initialize();
    await NotificationService().requestPermission();

    // ✅ INICIALIZAR FCM SERVICE (Push Notifications)
    // Isso registra o token FCM no servidor para receber push quando offline
    await FCMService().initialize();
    print('✅ FCMService inicializado');

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
  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addObserver(this);
  }

  @override
  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
    super.dispose();
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    print('🔄 App Lifecycle State: $state');

    switch (state) {
      case AppLifecycleState.resumed:
        // App voltou para foreground - marcar como ONLINE
        // ✅ Pode ter sido acordada pelo FCM (tela estava bloqueada)
        print(
          '☀️ App em foreground - status: online (pode ter sido acordada pelo FCM)',
        );

        // ✅ SEMPRE tentar reconectar (pode ter desconectado quando tela bloqueou)
        if (!ChatService.isWebSocketConnected()) {
          print('🔄 Reconectando WebSocket após app acordar...');
          ChatService.connect();
        }

        // Mudar para modo foreground (heartbeat normal)
        ChatService.setBackgroundMode(false);
        ChatService.setScreenLocked(false); // ✅ Tela desbloqueada

        // ✅ Aguardar um pouco para garantir que WebSocket conectou
        Future.delayed(Duration(milliseconds: 500), () {
          ChatService.sendPresence('online');
        });
        break;

      case AppLifecycleState.paused:
        // App foi para background - esconder "Online" mas manter WebSocket vivo
        // Heartbeat continua para receber mensagens em tempo real
        print('🌑 App em background - WebSocket vivo, UI escondida');
        ChatService.sendPresence('background');
        ChatService.setBackgroundMode(true);
        // ✅ Tela pode estar bloqueada ou não - não sabemos ainda
        break;

      case AppLifecycleState.inactive:
        // App está inativa (tela bloqueada ou notificação apareceu)
        print(
          '🔒 App inativa (tela bloqueada?) - parando heartbeats, confiando no FCM',
        );
        ChatService.setScreenLocked(
          true,
        ); // ✅ Tela bloqueada - parar heartbeats
        break;

      case AppLifecycleState.hidden:
        // App está oculta (similar a inactive)
        print('👁️ App oculta - parando heartbeats, confiando no FCM');
        ChatService.setScreenLocked(true);
        break;

      case AppLifecycleState.detached:
        // App está a ser fechada
        print('💀 App fechada - desconectando');
        ChatService.disconnect();
        break;
    }
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
