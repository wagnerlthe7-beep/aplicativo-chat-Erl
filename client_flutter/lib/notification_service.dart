import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:timezone/data/latest.dart' as tz;
import 'navigation_service.dart';
import 'chat_service.dart';

class NotificationService {
  static final NotificationService _instance = NotificationService._internal();
  factory NotificationService() => _instance;
  NotificationService._internal();

  final FlutterLocalNotificationsPlugin _notifications =
      FlutterLocalNotificationsPlugin();
  bool _initialized = false;

  // Canal de notificação para mensagens
  static const String _channelId = 'message_notifications';
  static const String _channelName =
      'SpeekJoy'; // Nome da app (aparece no topo)
  static const String _channelDescription = 'Notificações de novas mensagens';

  Future<void> initialize() async {
    if (_initialized) return;

    // Inicializar timezone
    tz.initializeTimeZones();

    // Configurações para Android
    // Usar o mesmo ícone da app (como WhatsApp faz)
    // O Android converterá automaticamente para monocromático quando necessário
    const AndroidInitializationSettings androidSettings =
        AndroidInitializationSettings('@mipmap/ic_launcher');

    // Configurações para iOS
    const DarwinInitializationSettings iosSettings =
        DarwinInitializationSettings(
          requestAlertPermission: true,
          requestBadgePermission: true,
          requestSoundPermission: true,
        );

    // Configurações de inicialização
    const InitializationSettings settings = InitializationSettings(
      android: androidSettings,
      iOS: iosSettings,
    );

    await _notifications.initialize(
      settings,
      onDidReceiveNotificationResponse: _onNotificationTapped,
      onDidReceiveBackgroundNotificationResponse:
          _onBackgroundNotificationTapped,
    );

    // Criar canal para Android
    await _createNotificationChannel();

    _initialized = true;
    print('🔔 Serviço de notificações inicializado');
  }

  Future<void> _createNotificationChannel() async {
    const AndroidNotificationChannel channel = AndroidNotificationChannel(
      _channelId,
      _channelName,
      description: _channelDescription,
      importance: Importance.high,
      enableVibration: true,
      playSound: true,
    );

    await _notifications
        .resolvePlatformSpecificImplementation<
          AndroidFlutterLocalNotificationsPlugin
        >()
        ?.createNotificationChannel(channel);
  }

  Future<void> showNewMessageNotification({
    required String senderName,
    required String messageContent,
    required String chatId,
    String? senderAvatar,
  }) async {
    if (!_initialized) {
      print('❌ Serviço de notificações não inicializado');
      return;
    }

    // Limitar o conteúdo da mensagem
    String previewContent = messageContent;
    if (previewContent.length > 50) {
      previewContent = '${previewContent.substring(0, 47)}...';
    }

    // Ações de resposta rápida (Android)
    // Ação de resposta com campo de texto inline
    const replyAction = AndroidNotificationAction(
      'reply_action',
      'Responder',
      showsUserInterface: false,
      cancelNotification: false,
    );

    const markAsReadAction = AndroidNotificationAction(
      'mark_read_action',
      'Marcar como lido',
      showsUserInterface: false,
    );

    // Detalhes da notificação para Android
    AndroidNotificationDetails androidDetails = AndroidNotificationDetails(
      _channelId,
      _channelName,
      channelDescription: _channelDescription,
      importance: Importance.high,
      priority: Priority.high,
      showWhen: true,
      enableVibration: true,
      playSound: true,
      icon: '@mipmap/ic_launcher', // Mesmo ícone da app (como WhatsApp)
      color: const Color(
        0xFF075E54,
      ), // Cor de fundo do ícone (verde escuro da app)
      largeIcon: senderAvatar != null
          ? FilePathAndroidBitmap(senderAvatar)
          : null,
      styleInformation: BigTextStyleInformation(
        previewContent,
        htmlFormatBigText: true,
        contentTitle:
            senderName, // Nome do remetente (ou número se não tiver nome)
        htmlFormatContentTitle: true,
      ),
      category: AndroidNotificationCategory.message,
      actions: [replyAction, markAsReadAction], // Ações de resposta rápida
      autoCancel: true,
    );

    // Detalhes da notificação para iOS
    DarwinNotificationDetails iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
      sound: 'default',
      badgeNumber: 1,
      subtitle: 'SpeekJoy', // Nome da app
    );

    // Detalhes gerais
    NotificationDetails notificationDetails = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    // Gerar ID único para a notificação
    int notificationId = DateTime.now().millisecondsSinceEpoch % 100000;

    await _notifications.show(
      notificationId,
      senderName, // Título: nome do remetente (ou número se não tiver nome) - já vem correto
      previewContent, // Corpo: conteúdo da mensagem
      notificationDetails,
      payload: 'chat_$chatId',
    );

    print('🔔 Notificação enviada: $senderName - $previewContent');
  }

  Future<void> showNotification({
    required int id,
    required String title,
    required String body,
    String? payload,
  }) async {
    if (!_initialized) return;

    AndroidNotificationDetails androidDetails = AndroidNotificationDetails(
      _channelId,
      _channelName,
      channelDescription: _channelDescription,
      importance: Importance.high,
      priority: Priority.high,
      showWhen: true,
    );

    DarwinNotificationDetails iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
    );

    NotificationDetails notificationDetails = NotificationDetails(
      android: androidDetails,
      iOS: iosDetails,
    );

    await _notifications.show(
      id,
      title,
      body,
      notificationDetails,
      payload: payload,
    );
  }

  Future<void> cancelNotification(int id) async {
    await _notifications.cancel(id);
  }

  Future<void> cancelAllNotifications() async {
    await _notifications.cancelAll();
  }

  Future<void> _onNotificationTapped(NotificationResponse response) async {
    await _handleNotificationResponse(response);
  }

  // Handler para notificações em background (deve ser top-level)
  @pragma('vm:entry-point')
  static Future<void> _onBackgroundNotificationTapped(
    NotificationResponse response,
  ) async {
    final service = NotificationService();
    await service._handleNotificationResponse(response);
  }

  Future<void> _handleNotificationResponse(
    NotificationResponse response,
  ) async {
    final payload = response.payload;
    final actionId = response.actionId;

    print('🔔 Notificação tocada: $payload, ação: $actionId');

    if (payload == null || !payload.startsWith('chat_')) {
      return;
    }

    final chatId = payload.substring(5); // Remove 'chat_' prefix

    // Se foi uma ação de resposta rápida
    if (actionId == 'reply_action') {
      final replyText = response.input;
      if (replyText != null && replyText.isNotEmpty) {
        print('💬 Resposta rápida: $replyText para $chatId');
        await _sendQuickReply(chatId, replyText);
      }
      return;
    }

    // Se foi ação de marcar como lido
    if (actionId == 'mark_read_action') {
      print('✅ Marcando como lido: $chatId');
      ChatService.markChatAsRead(chatId);
      return;
    }

    // Se foi apenas toque na notificação, navegar para o chat
    if (actionId == null || actionId.isEmpty) {
      print('🔔 Abrir chat: $chatId');
      await NavigationService.navigateToChat(chatId);
    }
  }

  Future<void> _sendQuickReply(String chatId, String message) async {
    try {
      // Enviar mensagem via WebSocket
      await ChatService.sendMessage(chatId, message);
      print('✅ Resposta rápida enviada: $message');
    } catch (e) {
      print('❌ Erro ao enviar resposta rápida: $e');
      // Se WebSocket não estiver conectado, salvar como pendente
      // O MessageSyncService vai enviar quando reconectar
    }
  }

  // Verificar permissões
  Future<bool> hasPermission() async {
    final androidPlugin = _notifications
        .resolvePlatformSpecificImplementation<
          AndroidFlutterLocalNotificationsPlugin
        >();
    if (androidPlugin != null) {
      final notificationsEnabled = await androidPlugin
          .areNotificationsEnabled();
      return notificationsEnabled ?? false;
    }
    return true; // iOS geralmente já tem permissão via initialize
  }

  // Solicitar permissões
  Future<bool> requestPermission() async {
    final androidPlugin = _notifications
        .resolvePlatformSpecificImplementation<
          AndroidFlutterLocalNotificationsPlugin
        >();
    if (androidPlugin != null) {
      final granted = await androidPlugin.requestNotificationsPermission();
      return granted ?? false;
    }
    return true; // iOS geralmente já tem permissão via initialize
  }

  // Obter permissões detalhadas (Android 13+)
  Future<bool> getPermissions() async {
    final androidPlugin = _notifications
        .resolvePlatformSpecificImplementation<
          AndroidFlutterLocalNotificationsPlugin
        >();
    if (androidPlugin != null) {
      final granted = await androidPlugin.requestNotificationsPermission();
      return granted ?? false;
    }
    return true; // iOS geralmente já tem permissão
  }
}
