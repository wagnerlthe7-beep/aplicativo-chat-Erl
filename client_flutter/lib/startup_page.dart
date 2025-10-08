import 'package:flutter/material.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';

class StartupPage extends StatefulWidget {
  @override
  _StartupPageState createState() => _StartupPageState();
}

class _StartupPageState extends State<StartupPage> {
  final _storage = FlutterSecureStorage();
  static const String backendUrl = 'http://10.0.2.2:4000';

  @override
  void initState() {
    super.initState();
    _validateAndNavigate();
  }

  Future<void> _validateAndNavigate() async {
    final accessToken = await _storage.read(key: 'access_token');
    
    if (accessToken == null) {
      // Não tem token - ir para login
      _goToWelcomePage();
      return;
    }

    // ✅✅✅ VERIFICAR NO BACKEND se a sessão ainda é válida
    final isValid = await _validateSessionWithBackend(accessToken);
    
    if (isValid) {
      // Sessão VÁLIDA - ir para chats
      _goToChatListPage();
    } else {
      // Sessão INVÁLIDA - limpar tokens e ir para login
      await _storage.delete(key: 'access_token');
      await _storage.delete(key: 'refresh_token');
      _goToWelcomePage();
    }
  }

  Future<bool> _validateSessionWithBackend(String accessToken) async {
    try {
      final url = Uri.parse('$backendUrl/auth/validate-session');
      final response = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'access_token': accessToken,
        }),
      );

      print('🔍 Session validation response: ${response.statusCode}');
      
      if (response.statusCode == 200) {
        final body = jsonDecode(response.body);
        return body['status'] == 'valid';
      } else {
        print('❌ Session validation failed: ${response.body}');
        return false;
      }
    } catch (e) {
      print('❌ Error validating session: $e');
      return false;
    }
  }

  void _goToChatListPage() {
    Navigator.pushReplacementNamed(context, '/chatList');
  }

  void _goToWelcomePage() {
    Navigator.pushReplacementNamed(context, '/welcome');
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            CircularProgressIndicator(),
            SizedBox(height: 20),
            Text(
              'Verificando sessão...',
              style: TextStyle(fontSize: 16),
            ),
          ],
        ),
      ),
    );
  }
}