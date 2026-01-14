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

  // ✅ NOVO MÉTODO: Obter mapa de contatos locais (Telefone -> Nome)
  static Future<Map<String, String>> getLocalContactsMap() async {
    final Map<String, String> contactsMap = {};
    try {
      // Verificar permissão
      if (!await FlutterContacts.requestPermission(readonly: true)) {
        print('❌ Permissão de contatos negada');
        return contactsMap;
      }

      // Buscar contatos com propriedades
      final contacts = await FlutterContacts.getContacts(withProperties: true);
      
      for (final contact in contacts) {
        for (final phone in contact.phones) {
          // Normalizar telefone (remover espaços, traços, parênteses)
          // Manter o '+' se existir, pois é importante para DDI
          final cleanPhone = phone.number.replaceAll(RegExp(r'[\s\-\(\)]'), '');
          
          if (cleanPhone.isNotEmpty) {
            contactsMap[cleanPhone] = contact.displayName;
            
            // Também guardar sem o '+' para garantir match
            if (cleanPhone.startsWith('+')) {
               contactsMap[cleanPhone.substring(1)] = contact.displayName;
            }
          }
        }
      }
      print('📱 ${contactsMap.length} contatos locais mapeados.');
    } catch (e) {
      print('❌ Erro ao mapear contatos locais: $e');
    }
    return contactsMap;
  }
}
