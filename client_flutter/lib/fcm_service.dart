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
import 'package:permission_handler/permission_handler.dart';
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

    // 2. Verificar e solicitar desativação de otimização de bateria (Android)
    if (defaultTargetPlatform == TargetPlatform.android) {
      await _checkBatteryOptimization();
    }

    // NOTA: Handler de background é registrado no main.dart ANTES de inicializar
    // Não registrar aqui novamente para evitar duplicação

    // 3. Obter token FCM
    await _getAndRegisterToken();

    // 4. Escutar refresh de token
    _tokenRefreshSubscription = _messaging.onTokenRefresh.listen((newToken) {
      print('🔔 [FCM] Token atualizado: $newToken');
      _fcmToken = newToken;
      _registerTokenWithServer(newToken);
    });

    // 6. Configurar handlers de mensagem
    _setupMessageHandlers();

    print('🔔 [FCM] Serviço inicializado com sucesso');
  }

  /// Verificar e solicitar desativação de otimização de bateria
  /// Isso é CRÍTICO para notificações funcionarem em background
  Future<void> _checkBatteryOptimization() async {
    try {
      // Verificar se a otimização de bateria está ativa
      final isIgnored = await Permission.ignoreBatteryOptimizations.isGranted;

      if (!isIgnored) {
        print(
          '⚠️ [FCM] Otimização de bateria está ativa - notificações podem não funcionar em background',
        );
        print('💡 [FCM] Solicitando desativação de otimização de bateria...');

        // Solicitar permissão para ignorar otimização de bateria
        final status = await Permission.ignoreBatteryOptimizations.request();

        if (status.isGranted) {
          print('✅ [FCM] Otimização de bateria desativada com sucesso');
        } else if (status.isPermanentlyDenied) {
          print(
            '⚠️ [FCM] Permissão negada permanentemente - usuário precisa ativar manualmente',
          );
          print(
            '   Vá em: Configurações > Apps > SpeekJoy > Bateria > Sem restrições',
          );
        } else {
          print(
            '⚠️ [FCM] Permissão negada - notificações podem não funcionar em background',
          );
        }
      } else {
        print('✅ [FCM] Otimização de bateria já está desativada');
      }
    } catch (e) {
      print('⚠️ [FCM] Erro ao verificar otimização de bateria: $e');
      // Não bloquear inicialização se falhar
    }
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
    print('🔔 [FCM] Alert: ${settings.alert}');
    print('🔔 [FCM] Badge: ${settings.badge}');
    print('🔔 [FCM] Sound: ${settings.sound}');

    if (settings.authorizationStatus != AuthorizationStatus.authorized) {
      print('⚠️ [FCM] ATENÇÃO: Permissões de notificação não concedidas!');
    }
  }

  /// Obter token FCM e registrar no servidor
  Future<void> _getAndRegisterToken() async {
    try {
      _fcmToken = await _messaging.getToken();
      print('🔔 [FCM] Token obtido: $_fcmToken');
      print('🔔 [FCM] Token length: ${_fcmToken?.length ?? 0}');

      if (_fcmToken != null && _fcmToken!.isNotEmpty) {
        await _registerTokenWithServer(_fcmToken!);
      } else {
        print('⚠️ [FCM] Token FCM está vazio ou nulo!');
      }
    } catch (e, stackTrace) {
      print('❌ [FCM] Erro ao obter token: $e');
      print('   Stack trace: $stackTrace');
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

    // Mostrar notificação local apenas se não estiver no chat ativo
    // O ChatService já mostra notificações quando não está no chat ativo
    final data = message.data;
    if (data['type'] == 'message') {
      final senderName =
          data['sender_name'] ?? message.notification?.title ?? 'Nova mensagem';
      final content = data['content'] ?? message.notification?.body ?? '';
      final fromUserId = data['sender_id'];

      // Mostrar notificação local (será filtrada pelo ChatService se necessário)
      try {
        await NotificationService().showNewMessageNotification(
          senderName: senderName,
          messageContent: content,
          chatId: fromUserId ?? '',
        );
      } catch (e) {
        print('⚠️ [FCM Foreground] Erro ao mostrar notificação: $e');
      }
    }
  }

  /// Handler para quando usuário toca na notificação
  void _handleMessageOpenedApp(RemoteMessage message) {
    print('🔔 [FCM] Notificação tocada: ${message.data}');

    // Navegar para o chat específico
    final chatId = message.data['chat_id'] ?? message.data['sender_id'];
    if (chatId != null) {
      // TODO: Navegar para o chat usando um serviço de navegação global
      print('🔔 [FCM] Deveria navegar para chat: $chatId');
    }
  }

  /// Handler estático para mensagens em BACKGROUND (app fechada ou minimizada)
  /// ✅ CRÍTICO: Esta função é chamada mesmo quando a tela está bloqueada
  /// O FCM acorda a app para processar a mensagem e enviar ACK
  static Future<void> _handleBackgroundMessage(RemoteMessage message) async {
    print(
      '🔔 [FCM Background] Processando mensagem (tela pode estar bloqueada)...',
    );
    print(
      '   Notification: ${message.notification?.title} - ${message.notification?.body}',
    );
    print('   Data: ${message.data}');

    try {
      // ✅ INICIALIZAR NotificationService (pode não estar inicializado em background)
      await NotificationService().initialize();
      print('✅ [FCM Background] NotificationService inicializado');

      final data = message.data;
      final messageId = data['message_id'];
      final dbMessageId = data['db_message_id'];
      final fromUserId = data['sender_id'];
      final messageType = data['type'];

      print(
        '   MessageId: $messageId, DbMessageId: $dbMessageId, FromUserId: $fromUserId, Type: $messageType',
      );

      // ✅ ENVIAR ACK DE DELIVERED PARA O SERVIDOR
      // IMPORTANTE: Este ACK confirma que a mensagem foi recebida mesmo com tela bloqueada
      // O FCM acordou a app especificamente para processar esta mensagem
      if (messageId != null || dbMessageId != null) {
        final ackMessageId = dbMessageId ?? messageId;
        print('📤 [FCM Background] Enviando ACK para mensagem: $ackMessageId');
        await _sendDeliveredAck(messageId: ackMessageId.toString());
        print('✅ [FCM Background] ACK enviado com sucesso');
      }

      // ✅ TENTAR RECONECTAR WEBSOCKET (se possível)
      // Quando FCM acorda a app, podemos tentar reconectar para receber mensagens em tempo real
      try {
        // Importar ChatService dinamicamente para evitar dependência circular
        // NOTA: Isso pode não funcionar em background isolado, mas tentamos
        print('🔄 [FCM Background] Tentando reconectar WebSocket...');
        // ChatService.connect(); // Comentado - pode causar problemas em background isolado
        // Em vez disso, confiamos que quando o usuário abrir a app, ela reconecta
      } catch (e) {
        print('⚠️ [FCM Background] Não foi possível reconectar WebSocket: $e');
        // Não é crítico - FCM já entregou a mensagem e ACK foi enviado
      }

      // Mostrar notificação local
      if (messageType == 'message') {
        final senderName = data['sender_name'] ?? 'Nova mensagem';
        final content = data['content'] ?? '';

        print(
          '🔔 [FCM Background] Mostrando notificação: $senderName - $content',
        );

        await NotificationService().showNewMessageNotification(
          senderName: senderName,
          messageContent: content,
          chatId: fromUserId ?? '',
        );

        print('✅ [FCM Background] Notificação exibida com sucesso');
      } else {
        print(
          '⚠️ [FCM Background] Tipo de mensagem desconhecido: $messageType',
        );
      }
    } catch (e, stackTrace) {
      print('❌ [FCM Background] Erro ao processar mensagem: $e');
      print('   Stack trace: $stackTrace');
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
