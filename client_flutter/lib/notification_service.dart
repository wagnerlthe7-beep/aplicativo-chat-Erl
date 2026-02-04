import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter_local_notifications/flutter_local_notifications.dart';
import 'package:timezone/timezone.dart' as tz;
import 'package:timezone/data/latest.dart' as tz;

class NotificationService {
  static final NotificationService _instance = NotificationService._internal();
  factory NotificationService() => _instance;
  NotificationService._internal();

  final FlutterLocalNotificationsPlugin _notifications =
      FlutterLocalNotificationsPlugin();
  bool _initialized = false;

  // Canal de notificação para mensagens
  static const String _channelId = 'message_notifications';
  static const String _channelName = 'Mensagens';
  static const String _channelDescription = 'Notificações de novas mensagens';

  Future<void> initialize() async {
    if (_initialized) return;

    // Inicializar timezone
    tz.initializeTimeZones();

    // Configurações para Android
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
      icon: '@mipmap/ic_launcher',
      largeIcon: senderAvatar != null
          ? FilePathAndroidBitmap(senderAvatar)
          : null,
      styleInformation: BigTextStyleInformation(
        previewContent,
        htmlFormatBigText: true,
        contentTitle: senderName,
        htmlFormatContentTitle: true,
      ),
      category: AndroidNotificationCategory.message,
    );

    // Detalhes da notificação para iOS
    DarwinNotificationDetails iosDetails = DarwinNotificationDetails(
      presentAlert: true,
      presentBadge: true,
      presentSound: true,
      sound: 'default',
      badgeNumber: 1,
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
      senderName,
      previewContent,
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
    final payload = response.payload;
    print('🔔 Notificação tocada: $payload');

    if (payload != null && payload.startsWith('chat_')) {
      final chatId = payload.substring(5); // Remove 'chat_' prefix
      print('🔔 Abrir chat: $chatId');

      // TODO: Navegar para a página do chat
      // Você pode usar um serviço de navegação ou passar uma callback
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
