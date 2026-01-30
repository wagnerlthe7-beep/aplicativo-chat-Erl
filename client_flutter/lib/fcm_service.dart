// fcm_service.dart
// Serviço de Firebase Cloud Messaging para push notifications e wake-up da app
//
// ARQUITETURA CORRETA (estilo WhatsApp):
// - Delivered = ACK técnico (independente de "online")
// - Push notification acorda a app mesmo fechada
// - App envia ACK ao servidor quando recebe mensagem
// - Servidor marca como "delivered" APENAS após receber ACK

import 'dart:async';
import 'dart:convert';
import 'package:firebase_messaging/firebase_messaging.dart';
import 'package:flutter/foundation.dart';
import 'package:http/http.dart' as http;
import 'auth_service.dart';
import 'notification_service.dart';

/// Handler para mensagens em background (DEVE ser top-level function)
/// Esta função é chamada quando a app está fechada ou em background
@pragma('vm:entry-point')
Future<void> firebaseMessagingBackgroundHandler(RemoteMessage message) async {
  print('🔔 [FCM Background] Mensagem recebida em background!');
  print('   Data: ${message.data}');

  // Processar mensagem e enviar ACK
  await FCMService._handleBackgroundMessage(message);
}

class FCMService {
  static final FCMService _instance = FCMService._internal();
  factory FCMService() => _instance;
  FCMService._internal();

  final FirebaseMessaging _messaging = FirebaseMessaging.instance;
  String? _fcmToken;
  StreamSubscription<String>? _tokenRefreshSubscription;

  // Base URL do servidor (mesmo do AuthService)
  // NOTA: Em produção, mudar para o domínio real (ex: https://speekjoy.com)
  static const String _baseUrl = 'http://192.168.100.35:4000';

  /// Token FCM atual
  String? get fcmToken => _fcmToken;

  /// Inicializar o serviço FCM
  Future<void> initialize() async {
    print('🔔 [FCM] Inicializando serviço...');

    // 1. Solicitar permissões
    await _requestPermissions();

    // 2. Configurar handler de background (ANTES de getToken)
    FirebaseMessaging.onBackgroundMessage(firebaseMessagingBackgroundHandler);

    // 3. Obter token FCM
    await _getAndRegisterToken();

    // 4. Escutar refresh de token
    _tokenRefreshSubscription = _messaging.onTokenRefresh.listen((newToken) {
      print('🔔 [FCM] Token atualizado: $newToken');
      _fcmToken = newToken;
      _registerTokenWithServer(newToken);
    });

    // 5. Configurar handlers de mensagem
    _setupMessageHandlers();

    print('🔔 [FCM] Serviço inicializado com sucesso');
  }

  /// Solicitar permissões de notificação
  Future<void> _requestPermissions() async {
    final settings = await _messaging.requestPermission(
      alert: true,
      announcement: false,
      badge: true,
      carPlay: false,
      criticalAlert: false,
      provisional: false,
      sound: true,
    );

    print('🔔 [FCM] Permissão: ${settings.authorizationStatus}');
  }

  /// Obter token FCM e registrar no servidor
  Future<void> _getAndRegisterToken() async {
    try {
      _fcmToken = await _messaging.getToken();
      print('🔔 [FCM] Token obtido: $_fcmToken');

      if (_fcmToken != null) {
        await _registerTokenWithServer(_fcmToken!);
      }
    } catch (e) {
      print('❌ [FCM] Erro ao obter token: $e');
    }
  }

  /// Registrar token FCM no servidor
  Future<void> _registerTokenWithServer(String token) async {
    try {
      final accessToken = await AuthService.getAccessToken();
      if (accessToken == null) {
        print('❌ [FCM] Sem access token para registrar FCM token');
        return;
      }

      final response = await http.post(
        Uri.parse('$_baseUrl/api/fcm/register'),
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: json.encode({
          'fcm_token': token,
          'device_type': defaultTargetPlatform == TargetPlatform.iOS
              ? 'ios'
              : 'android',
        }),
      );

      if (response.statusCode == 200) {
        print('✅ [FCM] Token registrado no servidor');
      } else {
        print('❌ [FCM] Erro ao registrar token: ${response.statusCode}');
      }
    } catch (e) {
      print('❌ [FCM] Erro ao registrar token: $e');
    }
  }

  /// Configurar handlers de mensagem
  void _setupMessageHandlers() {
    // Handler para mensagens em FOREGROUND
    FirebaseMessaging.onMessage.listen(_handleForegroundMessage);

    // Handler quando usuário toca na notificação (app em background)
    FirebaseMessaging.onMessageOpenedApp.listen(_handleMessageOpenedApp);

    // Verificar se app foi aberta por uma notificação
    _messaging.getInitialMessage().then((message) {
      if (message != null) {
        print('🔔 [FCM] App aberta por notificação: ${message.data}');
        _handleMessageOpenedApp(message);
      }
    });
  }

  /// Handler para mensagens em FOREGROUND
  Future<void> _handleForegroundMessage(RemoteMessage message) async {
    print('🔔 [FCM Foreground] Mensagem recebida!');
    print('   Title: ${message.notification?.title}');
    print('   Body: ${message.notification?.body}');
    print('   Data: ${message.data}');

    // Processar a mensagem
    await _processIncomingMessage(message);

    // Mostrar notificação local (opcional, já que a app está aberta)
    // O ChatService já mostra notificações quando não está no chat ativo
  }

  /// Handler para quando usuário toca na notificação
  void _handleMessageOpenedApp(RemoteMessage message) {
    print('🔔 [FCM] Notificação tocada: ${message.data}');

    // Navegar para o chat específico
    final chatId = message.data['chat_id'] ?? message.data['from'];
    if (chatId != null) {
      // TODO: Navegar para o chat usando um serviço de navegação global
      print('🔔 [FCM] Deveria navegar para chat: $chatId');
    }
  }

  /// Handler estático para mensagens em BACKGROUND (app fechada ou minimizada)
  static Future<void> _handleBackgroundMessage(RemoteMessage message) async {
    print('🔔 [FCM Background] Processando mensagem...');

    final data = message.data;
    final messageId = data['message_id'];
    final dbMessageId = data['db_message_id'];
    final fromUserId = data['from'];

    if (messageId != null || dbMessageId != null) {
      // ENVIAR ACK DE DELIVERED PARA O SERVIDOR
      // Isso marca a mensagem como "delivered" no servidor
      await _sendDeliveredAck(messageId: dbMessageId ?? messageId);
    }

    // Mostrar notificação local
    if (data['type'] == 'message') {
      final senderName = data['sender_name'] ?? 'Nova mensagem';
      final content = data['content'] ?? '';

      await NotificationService().showNewMessageNotification(
        senderName: senderName,
        messageContent: content,
        chatId: fromUserId ?? '',
      );
    }
  }

  /// Processar mensagem recebida (foreground ou background)
  Future<void> _processIncomingMessage(RemoteMessage message) async {
    final data = message.data;
    final messageId = data['message_id'];
    final dbMessageId = data['db_message_id'];

    if (messageId != null || dbMessageId != null) {
      // ENVIAR ACK DE DELIVERED
      await _sendDeliveredAck(messageId: dbMessageId ?? messageId);
    }
  }

  /// Enviar ACK de delivered para o servidor
  /// Esta é a função CRÍTICA que confirma recebimento da mensagem
  static Future<void> _sendDeliveredAck({required String messageId}) async {
    try {
      print('📤 [ACK] Enviando ACK de delivered para mensagem: $messageId');

      final accessToken = await AuthService.getAccessToken();
      if (accessToken == null) {
        print('❌ [ACK] Sem access token');
        return;
      }

      final response = await http.post(
        Uri.parse('$_baseUrl/api/messages/ack'),
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: json.encode({'message_id': messageId, 'status': 'delivered'}),
      );

      if (response.statusCode == 200) {
        print('✅ [ACK] ACK enviado com sucesso para mensagem: $messageId');
      } else {
        print('❌ [ACK] Erro ao enviar ACK: ${response.statusCode}');
      }
    } catch (e) {
      print('❌ [ACK] Erro ao enviar ACK: $e');
    }
  }

  /// Enviar ACK de delivered (método público para uso pelo ChatService)
  Future<void> sendDeliveredAck(String messageId) async {
    await _sendDeliveredAck(messageId: messageId);
  }

  /// Limpar recursos
  void dispose() {
    _tokenRefreshSubscription?.cancel();
  }

  /// Remover token do servidor (logout)
  Future<void> unregisterToken() async {
    try {
      final accessToken = await AuthService.getAccessToken();
      if (accessToken == null || _fcmToken == null) return;

      await http.post(
        Uri.parse('$_baseUrl/api/fcm/unregister'),
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: json.encode({'fcm_token': _fcmToken}),
      );

      print('✅ [FCM] Token removido do servidor');
    } catch (e) {
      print('❌ [FCM] Erro ao remover token: $e');
    }
  }
}
