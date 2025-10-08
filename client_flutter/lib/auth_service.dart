import 'dart:convert';
import 'dart:io';
import 'package:firebase_auth/firebase_auth.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';
import 'package:http/http.dart' as http;
import 'package:uuid/uuid.dart';

class AuthService {
  static final _auth = FirebaseAuth.instance;
  static final _storage = FlutterSecureStorage();
  static final _uuid = Uuid();

  // URL do backend Erlang
  static const String backendUrl = 'http://10.0.2.2:4000';

  /// -----------------------------
  /// 1) Device UUID (uma vez por instalação)
  /// -----------------------------
  static Future<String> getOrCreateDeviceId() async {
    var id = await _storage.read(key: 'device_uuid');
    if (id == null) {
      id = _uuid.v4();
      await _storage.write(key: 'device_uuid', value: id);
    }
    return id;
  }

  /// -----------------------------
  /// 2) Print do Firebase ID Token
  /// -----------------------------
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

  /// -----------------------------
  /// 3) Firebase Phone Auth - enviar SMS
  /// -----------------------------
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
        final userCredential = await _auth.signInWithCredential(credential);
        await afterFirebaseSignInBackend(userCredential: userCredential);
        autoRetrieved(userCredential);
      },
      verificationFailed: verificationFailed,
      codeSent: codeSent,
      codeAutoRetrievalTimeout: (String verificationId) {},
    );
  }

  /// -----------------------------
  /// 4) Firebase SMS code login
  /// -----------------------------
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
      await afterFirebaseSignInBackend(userCredential: userCredential);
      return true;
    } catch (e) {
      print('❌ Erro signInWithSmsCode: $e');
      return false;
    }
  }

  /// -----------------------------
  /// 5) Trocar ID token Firebase por sessão do backend
  /// -----------------------------
  static Future<void> afterFirebaseSignInBackend({
    UserCredential? userCredential,
  }) async {
    final user = userCredential?.user ?? _auth.currentUser;
    if (user == null) {
      print('❌ Nenhum usuário Firebase encontrado');
      throw Exception('Usuário Firebase não encontrado');
    }

    try {
      final idToken = await user.getIdToken();
      final deviceId = await getOrCreateDeviceId();
      final deviceInfo =
          '${Platform.operatingSystem} ${Platform.operatingSystemVersion}';

      print('🔄 Enviando dados para backend: device=$deviceId, phone=${user.phoneNumber}');

      final url = Uri.parse('$backendUrl/auth/firebase');
      final res = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'idToken': idToken,
          'phone': user.phoneNumber,
          'device_uuid': deviceId,
          'device_info': deviceInfo,
        }),
      );

      print('📡 Backend response: ${res.statusCode}');

      if (res.statusCode == 200) {
        final body = jsonDecode(res.body);
        // Backend retorna access_token + refresh_token
        await _storage.write(key: 'access_token', value: body['access_token']);
        await _storage.write(key: 'refresh_token', value: body['refresh_token']);
        print('✅ Tokens salvos com sucesso');
      } else {
        print('❌ Backend auth falhou: ${res.statusCode} ${res.body}');
        throw Exception('Backend auth falhou: ${res.statusCode} ${res.body}');
      }
    } catch (e) {
      print('❌ Erro em afterFirebaseSignInBackend: $e');
      rethrow;
    }
  }

  /// -----------------------------
  /// 6) Refresh de tokens
  /// -----------------------------
  static Future<bool> tryRefresh() async {
    final refresh = await _storage.read(key: 'refresh_token');
    if (refresh == null) return false;

    final deviceId = await getOrCreateDeviceId();
    final url = Uri.parse('$backendUrl/auth/refresh');
    final res = await http.post(
      url,
      headers: {'Content-Type': 'application/json'},
      body: jsonEncode({'refresh_token': refresh, 'device_info': deviceId}),
    );

    if (res.statusCode == 200) {
      final body = jsonDecode(res.body);
      await _storage.write(key: 'access_token', value: body['access_token']);
      await _storage.write(key: 'refresh_token', value: body['refresh_token']);
      return true;
    } else {
      await _storage.delete(key: 'access_token');
      await _storage.delete(key: 'refresh_token');
      return false;
    }
  }

  /// -----------------------------
  /// 7) Logout - revogar sessão atual
  /// -----------------------------
  static Future<void> logout() async {
    final refresh = await _storage.read(key: 'refresh_token');
    if (refresh != null) {
      final url = Uri.parse('$backendUrl/auth/logout');
      await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({'refresh_token': refresh}),
      );
    }
    // Limpa storage
    await _storage.delete(key: 'access_token');
    await _storage.delete(key: 'refresh_token');
  }
}
