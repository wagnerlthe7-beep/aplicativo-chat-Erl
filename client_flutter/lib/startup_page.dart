import 'package:flutter/material.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import 'dart:convert';
import 'auth_service.dart';

class StartupPage extends StatefulWidget {
  @override
  _StartupPageState createState() => _StartupPageState();
}

class _StartupPageState extends State<StartupPage> {
  final _storage = FlutterSecureStorage();
  //static const String backendUrl = 'http://10.0.2.2:4000';
  static const String backendUrl = 'http://192.168.100.35:4000';

  @override
  void initState() {
    super.initState();
    _bootstrap();
  }

  Future<void> _bootstrap() async {
    final accessToken = await _storage.read(key: 'access_token');

    if (accessToken == null) {
      _goToWelcomePage();
      return;
    }

    // ✅ ENTRA IMEDIATAMENTE (Modelo WhatsApp - Offline First)
    _goToChatListPage();

    // 🔄 valida em background (não bloqueia UI)
    _validateSessionInBackground(accessToken);
  }

  Future<void> _validateSessionInBackground(String accessToken) async {
    try {
      // Usar a URL centralizada do app
      final url = Uri.parse('${AuthService.backendUrl}/auth/validate-session');
      final response = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({'access_token': accessToken}),
      ).timeout(const Duration(seconds: 8));

      if (response.statusCode == 401) {
        final body = jsonDecode(response.body);
        final error = body['error'];

        if (error == 'session_revoked' || error == 'session_not_found') {
          print('🚫 Sessão revogada ou não encontrada. Redirecionando...');
          await AuthService.clearLocalSession();

          if (mounted) {
            Navigator.pushReplacementNamed(context, '/welcome');
          }
        }
      }
    } catch (e) {
      // 🌐 erro de rede → IGNORA
      print('⚠️ Backend indisponível, mantendo sessão ativa');
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
      body: SizedBox.shrink(), // ✅ Tela limpa enquanto decide para onde ir
    );
  }
}
