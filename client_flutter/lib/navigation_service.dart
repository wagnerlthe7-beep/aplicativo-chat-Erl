import 'package:flutter/material.dart';
import 'chat_page.dart';
import 'chat_model.dart';
import 'chat_service.dart';
import 'notification_service.dart';

/// Serviço global de navegação para abrir chats a partir de notificações
class NavigationService {
  static final GlobalKey<NavigatorState> navigatorKey = GlobalKey<NavigatorState>();

  static NavigatorState? get navigator => navigatorKey.currentState;

  /// Navegar para um chat específico a partir de uma notificação
  static Future<void> navigateToChat(String chatId) async {
    print('🔔 [Navigation] Navegando para chat: $chatId');

    final navigator = NavigationService.navigator;
    if (navigator == null) {
      print('⚠️ [Navigation] Navigator não disponível ainda');
      return;
    }

    try {
      // Buscar informações do chat
      final chats = ChatService.currentChatList;
      final chat = chats.firstWhere(
        (c) => c.contactId == chatId,
        orElse: () => ChatContact(
          contactId: chatId,
          name: 'Desconhecido',
          lastMessage: '',
          lastMessageTime: DateTime.now(),
          unreadCount: 0,
        ),
      );

      // ✅ Marcar como lido
      ChatService.markChatAsRead(chatId);

      // ✅ Cancelar TODAS as notificações deste chat
      // Quando o usuário abre o chat através de uma notificação,
      // todas as outras notificações do mesmo chat devem ser canceladas
      await NotificationService().cancelChatNotifications(chatId);

      // Navegar para o chat
      navigator.push(
        MaterialPageRoute(
          builder: (context) => ChatPage(
            contact: chat,
            remoteUserId: chatId,
          ),
        ),
      );

      print('✅ [Navigation] Chat aberto: ${chat.name}');
    } catch (e) {
      print('❌ [Navigation] Erro ao navegar para chat: $e');
      // Se falhar, pelo menos navegar para a lista de chats
      navigator.pushReplacementNamed('/chatList');
    }
  }
}

