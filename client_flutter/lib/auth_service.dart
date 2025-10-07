import 'dart:convert';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;

class AuthService {
  static final _auth = FirebaseAuth.instance;
  static final _storage = FlutterSecureStorage();
  // Põe aqui a URL do teu backend Erlang que irá aceitar o token Firebase
  static const String backendUrl = 'http://10.0.2.2:4000';

  // ✅ PARA IMPRIMIR ID TOKEN
  static Future<void> printFirebaseIdToken() async {
    final User? user = FirebaseAuth.instance.currentUser;

    if (user != null) {
      try {
        final idTokenResult = await user.getIdTokenResult();
        final String? idToken = idTokenResult.token;

        if (idToken != null) {
          print('✅ Firebase ID Token: $idToken');
        } else {
          print('⚠️ ID Token is null.');
        }
      } catch (e) {
        print('❌ Erro ao obter ID Token: $e');
      }
    } else {
      print('⚠️ Nenhum usuário logado no momento.');
    }
  }

  // inicia verificação (envia SMS)
  static Future<void> verifyPhoneNumber({
    required String phoneNumber,
    required void Function(String verificationId, int? resendToken) codeSent,
    required void Function(UserCredential userCredential) autoRetrieved,
    required void Function(FirebaseAuthException e) verificationFailed,
  }) async {
    await _auth.verifyPhoneNumber(
      phoneNumber: phoneNumber,
      timeout: const Duration(seconds: 60),
      verificationCompleted: (PhoneAuthCredential credential) async {
        // assinatura automática (Android)
        final userCredential = await _auth.signInWithCredential(credential);
        await _afterFirebaseSignIn(userCredential);
        autoRetrieved(userCredential);
      },
      verificationFailed: verificationFailed,
      codeSent: (String verificationId, int? resendToken) {
        codeSent(verificationId, resendToken);
      },
      codeAutoRetrievalTimeout: (String verificationId) {},
    );
  }

  // login usando o código manualmente
  static Future<bool> signInWithSmsCode({
    required String verificationId,
    required String smsCode,
  }) async {
    try {
      final credential = PhoneAuthProvider.credential(
        verificationId: verificationId,
        smsCode: smsCode,
      );
      final userCredential = await _auth.signInWithCredential(credential);
      await _afterFirebaseSignIn(userCredential);
      return true;
    } catch (e) {
      print('Erro signInWithSmsCode: $e');
      return false;
    }
  }

  // troca o ID token Firebase com teu backend -> backend cria/retorna sessão própria
  static Future<void> _afterFirebaseSignIn(
    UserCredential userCredential,
  ) async {
    final user = userCredential.user;
    if (user == null) return;
    final idToken = await user.getIdToken();

    // chama teu backend: por exemplo POST /auth/firebase {idToken}
    final url = Uri.parse('$backendUrl/auth/firebase');
    final res = await http.post(
      url,
      headers: {'Content-Type': 'application/json'},
      body: jsonEncode({'idToken': idToken, 'phone': user.phoneNumber}),
    );

    if (res.statusCode == 200) {
      final body = jsonDecode(res.body);
      final apiToken = body['token']; // supomos que teu backend retorna token
      // salva token de sessão em storage seguro
      await _storage.write(key: 'api_token', value: apiToken);
      // podes salvar mais dados se quiseres
    } else {
      // lidar com erro (backend pode criar o user)
      throw Exception('Backend auth falhou: ${res.statusCode} ${res.body}');
    }
  }
}
