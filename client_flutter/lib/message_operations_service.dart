/// message_operations_service.dart
/// Serviço para operações avançadas de mensagens (editar, deletar, responder)
import 'dart:convert';
import 'package:http/http.dart' as http;
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'auth_service.dart';

class MessageOperationsService {
  static const _secureStorage = FlutterSecureStorage();

  /// URL base do backend
  static String get _backendUrl => AuthService.backendUrl;

  /// ======================
  /// 1. EDITAR MENSAGEM
  /// ======================
  static Future<Map<String, dynamic>> editMessage(
    dynamic messageId, // Aceitar int ou String
    String newContent,
  ) async {
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (accessToken == null || currentUserId == null) {
        throw Exception('Usuário não autenticado');
      }

      // Converter para String se for int
      final currentUserIdString = currentUserId.toString();
      final messageIdString = messageId.toString();

      final url = Uri.parse('$_backendUrl/api/messages/$messageIdString/edit');

      final response = await http.patch(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: jsonEncode({
          'content': newContent,
          'user_id': currentUserIdString,
        }),
      );

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        return {
          'success': true,
          'message': data['message'],
          'edited_message': data['edited_message'],
        };
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['error'] ?? 'Erro ao editar mensagem');
      }
    } catch (e) {
      print('❌ Erro ao editar mensagem: $e');
      return {'success': false, 'error': e.toString()};
    }
  }

  /// ======================
  /// 2. APAGAR MENSAGEM
  /// ======================
  static Future<Map<String, dynamic>> deleteMessage(
    dynamic messageId, { // Aceitar int ou String
    String reason = 'user_deleted',
  }) async {
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (accessToken == null || currentUserId == null) {
        throw Exception('Usuário não autenticado');
      }

      // Converter para String se for int
      final currentUserIdString = currentUserId.toString();
      final messageIdString = messageId.toString();

      final url = Uri.parse(
        '$_backendUrl/api/messages/$messageIdString/delete',
      );

      final response = await http.delete(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: jsonEncode({'user_id': currentUserIdString, 'reason': reason}),
      );

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        return {
          'success': true,
          'message': data['message'],
          'deleted_message': data['deleted_message'],
        };
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['error'] ?? 'Erro ao apagar mensagem');
      }
    } catch (e) {
      print('❌ Erro ao apagar mensagem: $e');
      return {'success': false, 'error': e.toString()};
    }
  }

  /// ======================
  /// 3. RESPONDER MENSAGEM
  /// ======================
  static Future<Map<String, dynamic>> replyToMessage(
    dynamic originalMessageId,
    String content, {
    required String receiverId,
  }) async {
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (accessToken == null || currentUserId == null) {
        throw Exception('Usuário não autenticado');
      }

      final url = Uri.parse(
        '$_backendUrl/api/messages/$originalMessageId/reply',
      );

      print('📤 Enviando reply para API:');
      print('   URL: $url');
      print('   sender_id: $currentUserId');
      print('   receiver_id: $receiverId');
      print('   content: $content');

      final Map<String, dynamic> requestBody = {
        'sender_id': currentUserId,
        'content': content,
        'receiver_id': receiverId, // ✅ CRÍTICO: sempre enviar receiver_id
      };

      final response = await http.post(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: jsonEncode(requestBody),
      );

      print('📥 Resposta do servidor:');
      print('   Status: ${response.statusCode}');
      print('   Body: ${response.body}');

      if (response.statusCode == 201) {
        final data = jsonDecode(response.body);
        print('✅ Reply enviado com sucesso');
        print('   Data: $data');
        return data;
      } else {
        final errorData = jsonDecode(response.body);
        print('❌ Erro no servidor: $errorData');
        throw Exception(
          errorData['error'] ?? 'Erro ao enviar reply (${response.statusCode})',
        );
      }
    } catch (e, stackTrace) {
      print('❌ ERRO CRÍTICO NO replyToMessage:');
      print('   Exception: $e');
      print('   Stack trace: $stackTrace');
      rethrow;
    }
  }

  /// ======================
  /// 4. HISTÓRICO DE EDIÇÕES
  /// ======================
  static Future<Map<String, dynamic>> getEditHistory(String messageId) async {
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');

      if (accessToken == null) {
        throw Exception('Usuário não autenticado');
      }

      final url = Uri.parse('$_backendUrl/api/messages/$messageId/history');

      final response = await http.get(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
      );

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        return {
          'success': true,
          'message_id': data['message_id'],
          'edit_history': data['edit_history'],
        };
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['error'] ?? 'Erro ao buscar histórico');
      }
    } catch (e) {
      print('❌ Erro ao buscar histórico de edições: $e');
      return {'success': false, 'error': e.toString()};
    }
  }

  /// ======================
  /// 5. RECUPERAR MENSAGEM (ADMIN)
  /// ======================
  static Future<Map<String, dynamic>> adminRecoverMessage(
    String messageId,
  ) async {
    try {
      final accessToken = await _secureStorage.read(key: 'access_token');
      final currentUserId = await _secureStorage.read(key: 'user_id');

      if (accessToken == null || currentUserId == null) {
        throw Exception('Usuário não autenticado');
      }

      // Converter para String se for int
      final currentUserIdString = currentUserId.toString();

      final url = Uri.parse(
        '$_backendUrl/api/admin/messages/$messageId/recover',
      );

      final response = await http.post(
        url,
        headers: {
          'Content-Type': 'application/json',
          'Authorization': 'Bearer $accessToken',
        },
        body: jsonEncode({'admin_id': currentUserIdString}),
      );

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        return {
          'success': true,
          'message': data['message'],
          'recovered_message': data['recovered_message'],
        };
      } else {
        final errorData = jsonDecode(response.body);
        throw Exception(errorData['error'] ?? 'Erro ao recuperar mensagem');
      }
    } catch (e) {
      print('❌ Erro ao recuperar mensagem: $e');
      return {'success': false, 'error': e.toString()};
    }
  }

  /// ======================
  /// 6. FUNÇÕES AUXILIARES
  /// ======================

  /// Verificar se mensagem pode ser editada (tempo limite)
  static bool canEditMessage(DateTime messageTime) {
    final now = DateTime.now();
    final difference = now.difference(messageTime);

    // Permitir edição por até 15 minutos
    return difference.inMinutes <= 15;
  }

  /// Verificar se mensagem pode ser apagada
  static bool canDeleteMessage(DateTime messageTime, bool isSender) {
    final now = DateTime.now();
    final difference = now.difference(messageTime);

    // Remetente pode apagar até 1 hora, destinatário pode apagar sempre
    if (isSender) {
      return difference.inHours <= 1;
    } else {
      return true;
    }
  }

  /// Formatar contagem de edições
  static String formatEditCount(int count) {
    if (count == 0) return '';
    if (count == 1) return 'editada';
    return 'editada $count vezes';
  }

  /// Verificar se mensagem é uma resposta
  static bool isReply(Map<String, dynamic> message) {
    return message['reply_to_id'] != null && message['reply_to_id'] != '';
  }

  /// Obter preview da mensagem respondida
  static String getReplyPreview(Map<String, dynamic> message, int maxLength) {
    final content = message['content']?.toString() ?? '';
    if (content.length <= maxLength) return content;
    return '${content.substring(0, maxLength - 3)}...';
  }
}
