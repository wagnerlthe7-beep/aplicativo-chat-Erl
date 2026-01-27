import 'dart:async';
import 'package:flutter/material.dart';
import 'package:flutter_foreground_task/flutter_foreground_task.dart';
import 'chat_service.dart';

@pragma('vm:entry-point')
void onForegroundTaskStart() {
  // Task que roda em background
  FlutterForegroundTask.setTaskHandler(WebSocketTaskHandler());
}

class WebSocketForegroundService {
  static const String channelId = 'websocket_service';
  static const String channelName = 'Serviço de Conexão';
  static const int notificationId = 888;

  static Future<void> initialize() async {
    // Inicializar o serviço foreground
    FlutterForegroundTask.init(
      androidNotificationOptions: AndroidNotificationOptions(
        channelId: channelId,
        channelName: channelName,
        channelDescription: 'Mantendo conexão WebSocket ativa',
        channelImportance: NotificationChannelImportance.LOW,
        priority: NotificationPriority.LOW,
      ),
      iosNotificationOptions: IOSNotificationOptions(
        showNotification: true,
        playSound: false,
      ),
      foregroundTaskOptions: ForegroundTaskOptions(
        eventAction: ForegroundTaskEventAction.repeat(5000), // 5 segundos
        autoRunOnBoot: true,
        allowWakeLock: true,
        allowWifiLock: true,
      ),
    );
  }

  static Future<void> startService() async {
    // Verificar permissões
    final isIgnoring =
        await FlutterForegroundTask.isIgnoringBatteryOptimizations;
    if (!isIgnoring) {
      await FlutterForegroundTask.requestIgnoreBatteryOptimization();
    }

    // Iniciar o serviço foreground
    await FlutterForegroundTask.startService(
      notificationTitle: 'Conexão Ativa',
      notificationText:
          'Mantendo WebSocket conectado para delivery de mensagens',
      callback: onForegroundTaskStart,
    );
  }

  static Future<void> stopService() async {
    await FlutterForegroundTask.stopService();
  }

  static Future<void> updateNotification(String title, String text) async {
    await FlutterForegroundTask.updateService(
      notificationTitle: title,
      notificationText: text,
    );
  }
}

class WebSocketTaskHandler extends TaskHandler {
  @override
  Future<void> onStart(DateTime timestamp, TaskStarter starter) async {
    // Iniciar quando o serviço começar
    print('🔌 WebSocket Foreground Service iniciado');

    // Enviar heartbeat periódico
    await _startPeriodicHeartbeat();
  }

  @override
  void onRepeatEvent(DateTime timestamp) {
    // Evento periódico a cada 5 segundos
    _sendHeartbeat();
  }

  @override
  Future<void> onDestroy(DateTime timestamp, bool isTerminated) async {
    // Limpar quando o serviço for destruído
    print('🔌 WebSocket Foreground Service destruído');
  }

  Future<void> _startPeriodicHeartbeat() async {
    // Implementar heartbeat periódico se necessário
  }

  Future<void> _sendHeartbeat() async {
    // Enviar heartbeat para manter WebSocket vivo
    try {
      // ✅ Verificar conexão antes de enviar heartbeat
      // O sendHeartbeat já verifica internamente, mas garantimos aqui também
      if (ChatService.isWebSocketConnected()) {
        print('💓 Heartbeat do Foreground Service');
        await ChatService.sendHeartbeat();
      } else {
        // WebSocket não conectado - não enviar heartbeat
        return;
      }
    } catch (e) {
      print('❌ Erro no heartbeat: $e');
    }
  }
}

// Classe para gerenciar o ciclo de vida do app com foreground service
class AppLifecycleManager with WidgetsBindingObserver {
  static final AppLifecycleManager _instance = AppLifecycleManager._internal();
  factory AppLifecycleManager() => _instance;
  AppLifecycleManager._internal();

  bool _isServiceRunning = false;

  void initialize() {
    WidgetsBinding.instance.addObserver(this);
  }

  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
  }

  @override
  void didChangeAppLifecycleState(AppLifecycleState state) {
    print('🔄 App Lifecycle State changed to: $state');

    switch (state) {
      case AppLifecycleState.paused:
        _handleAppPaused();
        break;
      case AppLifecycleState.resumed:
        _handleAppResumed();
        break;
      case AppLifecycleState.detached:
        _handleAppDetached();
        break;
      default:
        break;
    }
  }

  void _handleAppPaused() async {
    print('🌑 App em Background -> Iniciando Foreground Service');

    // Iniciar foreground service para manter WebSocket ativo
    if (!_isServiceRunning) {
      await WebSocketForegroundService.startService();
      _isServiceRunning = true;

      // Atualizar notificação
      await WebSocketForegroundService.updateNotification(
        'Conexão Ativa',
        'WebSocket mantido ativo para delivery de mensagens',
      );
    }
  }

  void _handleAppResumed() async {
    print('☀️ App em Foreground -> Parando Foreground Service');

    // Parar foreground service quando app volta para foreground
    if (_isServiceRunning) {
      await WebSocketForegroundService.stopService();
      _isServiceRunning = false;
    }

    // Reconectar WebSocket normalmente
    // (isso será feito pelo ChatService.connect())
  }

  void _handleAppDetached() async {
    print('💀 App sendo destruído -> Parando Foreground Service');

    // Garantir que o serviço pare quando app é fechado
    if (_isServiceRunning) {
      await WebSocketForegroundService.stopService();
      _isServiceRunning = false;
    }
  }
}
