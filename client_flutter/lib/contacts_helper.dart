import 'dart:convert';
import 'package:flutter_contacts/flutter_contacts.dart';
import 'package:http/http.dart' as http;
import 'auth_service.dart';

class ContactsHelper {
  /// Busca SIMPLES - envia o telefone exato como está
  static Future<String?> fetchBackendUserId(Contact contact) async {
    if (contact.phones.isEmpty) {
      print('❌ Contato sem número: ${contact.displayName}');
      return null;
    }

    final phone = contact.phones.first.number;
    print('🔍 Verificando telefone: "$phone"');

    try {
      // Usar o endpoint EXISTENTE /users/lookup
      final url = Uri.parse('${AuthService.backendUrl}/users/lookup');
      final response = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'phones': [phone], // Enviar o telefone EXATO como está
        }),
      );

      print('📡 Response: ${response.statusCode}');
      print('📡 Body: ${response.body}');

      if (response.statusCode == 200) {
        final data = jsonDecode(response.body);
        final users = data['users'] as List<dynamic>? ?? [];

        if (users.isNotEmpty) {
          final user = users.first;
          final userId = user['id']?.toString();
          print('✅ Usuário encontrado: $userId');
          return userId;
        } else {
          print('❌ Nenhum usuário encontrado para: $phone');
          return null;
        }
      } else {
        print('❌ Erro no backend: ${response.statusCode}');
        return null;
      }
    } catch (e) {
      print('❌ Erro: $e');
      return null;
    }
  }
}
