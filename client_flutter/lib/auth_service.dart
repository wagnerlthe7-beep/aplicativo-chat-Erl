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
  //static const String backendUrl = 'http://192.168.100.17:4000';

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
  /// 4) Firebase SMS code login (SEM finalizar backend automaticamente)
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
      // REMOVIDO: await afterFirebaseSignInBackend(userCredential: userCredential);
      // Agora o backend será chamado apenas quando necessário
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

      print(
        '🔄 Enviando dados para backend: device=$deviceId, phone=${user.phoneNumber}',
      );

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
        // Backend retorna access_token + refresh_token + user
        await _storage.write(key: 'access_token', value: body['access_token']);
        await _storage.write(
          key: 'refresh_token',
          value: body['refresh_token'],
        );
        if (body['user'] != null && body['user']['id'] != null) {
          await _storage.write(
            key: 'user_id',
            value: body['user']['id'].toString(),
          );
        }
        print('✅ Tokens e user_id salvos com sucesso');
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

  /// -----------------------------
  /// 8) Revogar outras sessões
  /// -----------------------------
  static Future<bool> revokeOtherSessions() async {
    final accessToken = await _storage.read(key: 'access_token');
    final deviceId = await getOrCreateDeviceId();

    if (accessToken == null) {
      print('❌ Nenhum access token encontrado');
      return false;
    }

    try {
      print('🚫 Revogando outras sessões...');

      // USAR O ENDPOINT CORRETO: /auth/revoke-others
      final url = Uri.parse('$backendUrl/auth/revoke-others');
      final res = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'access_token': accessToken,
          'device_uuid': deviceId,
        }),
      );

      print('📡 Revoke others response: ${res.statusCode} ${res.body}');

      if (res.statusCode == 200) {
        print('✅ Outras sessões revogadas com sucesso!');
        // NÃO limpa o storage - mantém a sessão ATUAL
        return true;
      } else {
        print('❌ Falha ao revogar outras sessões: ${res.statusCode}');
        return false;
      }
    } catch (e) {
      print('❌ Erro ao revogar outras sessões: $e');
      return false;
    }
  }

  /// -----------------------------
  /// 9) NOVA: Validar sessão com backend
  /// -----------------------------
  static Future<bool> validateCurrentSession() async {
    final accessToken = await _storage.read(key: 'access_token');

    if (accessToken == null) {
      return false;
    }

    try {
      final url = Uri.parse('$backendUrl/auth/validate-session');
      final response = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({'access_token': accessToken}),
      );

      if (response.statusCode == 200) {
        // Sessão válida
        return true;
      }

      if (response.statusCode == 401) {
        // Tentar entender se foi realmente revogada
        try {
          final body = jsonDecode(response.body);
          final error = body['error']?.toString();

          if (error == 'session_revoked' || error == 'session_not_found') {
            // Apenas aqui consideramos a sessão inválida de verdade
            return false;
          }
        } catch (_) {
          // Ignora erros de parse e trata como erro genérico
        }
      }

      // Para outros códigos (500, 400, erro inesperado), NÃO derruba sessão.
      // Apenas loga e considera ainda válida do ponto de vista do app.
      print(
        '⚠️ validateCurrentSession: status inesperado ${response.statusCode} ${response.body}',
      );
      return true;
    } catch (e) {
      // Importante: erro de rede NÃO deve matar a sessão.
      print('❌ Error validating session (mantendo sessão): $e');
      return true;
    }
  }

  /// -----------------------------
  /// 10) NOVA: Limpar sessão localmente
  /// -----------------------------
  static Future<void> clearLocalSession() async {
    await _storage.delete(key: 'access_token');
    await _storage.delete(key: 'refresh_token');
    print('✅ Local session cleared');
  }

  /// -----------------------------
  /// 11) NOVA: Verificar se é usuário novo
  /// -----------------------------
  static Future<bool> isNewUser() async {
    final user = _auth.currentUser;
    if (user == null) return true;

    try {
      final idToken = await user.getIdToken();
      final deviceId = await getOrCreateDeviceId();
      final deviceInfo =
          '${Platform.operatingSystem} ${Platform.operatingSystemVersion}';

      print('🔍 Verificando se é usuário novo...');

      final url = Uri.parse('$backendUrl/auth/check-user');
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

      print('📡 Check user response: ${res.statusCode}');

      if (res.statusCode == 200) {
        final body = jsonDecode(res.body);
        final isNew = body['is_new_user'] ?? true;
        print('✅ Usuário novo: $isNew');
        return isNew;
      } else {
        print('❌ Erro ao verificar usuário: ${res.statusCode} ${res.body}');
        return true; // Assume que é novo em caso de erro
      }
    } catch (e) {
      print('❌ Erro em isNewUser: $e');
      return true; // Assume que é novo em caso de erro
    }
  }

  /// -----------------------------
  /// 12) Completar login de usuário existente
  /// -----------------------------
  static Future<bool> completeExistingUserLogin() async {
    final user = _auth.currentUser;
    if (user == null) {
      print('❌ Nenhum usuário Firebase encontrado');
      return false;
    }

    try {
      final idToken = await user.getIdToken();
      final deviceId = await getOrCreateDeviceId();
      final deviceInfo =
          '${Platform.operatingSystem} ${Platform.operatingSystemVersion}';

      print('🔄 Finalizando login de usuário existente...');

      final url = Uri.parse('$backendUrl/auth/firebase');
      final res = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'idToken': idToken,
          'phone': user.phoneNumber,
          'device_uuid': deviceId,
          'device_info': deviceInfo,
          // Não enviar user_name para usuários existentes
        }),
      );

      print('📡 Backend response: ${res.statusCode}');

      if (res.statusCode == 200) {
        final body = jsonDecode(res.body);
        await _storage.write(key: 'access_token', value: body['access_token']);
        await _storage.write(
          key: 'refresh_token',
          value: body['refresh_token'],
        );
        print('✅ Login de usuário existente finalizado');
        return true;
      } else {
        print('❌ Backend auth falhou: ${res.statusCode} ${res.body}');
        return false;
      }
    } catch (e) {
      print('❌ Erro em completeExistingUserLogin: $e');
      return false;
    }
  }

  /// -----------------------------
  /// 13) Finalizar cadastro com nome do usuário
  /// -----------------------------
  static Future<bool> completeRegistrationWithName(String userName) async {
    final user = _auth.currentUser;
    if (user == null) {
      print('❌ Nenhum usuário Firebase encontrado');
      return false;
    }

    try {
      final idToken = await user.getIdToken();
      final deviceId = await getOrCreateDeviceId();
      final deviceInfo =
          '${Platform.operatingSystem} ${Platform.operatingSystemVersion}';

      print(
        '🔄 Finalizando cadastro com nome: $userName, device=$deviceId, phone=${user.phoneNumber}',
      );

      final url = Uri.parse('$backendUrl/auth/firebase');
      final res = await http.post(
        url,
        headers: {'Content-Type': 'application/json'},
        body: jsonEncode({
          'idToken': idToken,
          'phone': user.phoneNumber,
          'device_uuid': deviceId,
          'device_info': deviceInfo,
          'user_name': userName,
        }),
      );

      print('📡 Backend response: ${res.statusCode}');

      if (res.statusCode == 200) {
        final body = jsonDecode(res.body);
        // Backend retorna access_token + refresh_token
        await _storage.write(key: 'access_token', value: body['access_token']);
        await _storage.write(
          key: 'refresh_token',
          value: body['refresh_token'],
        );
        print('✅ Cadastro finalizado com sucesso');
        return true;
      } else {
        print('❌ Backend auth falhou: ${res.statusCode} ${res.body}');
        return false;
      }
    } catch (e) {
      print('❌ Erro em completeRegistrationWithName: $e');
      return false;
    }
  }

  /// Retorna o user_id salvo após login
  static Future<String?> getCurrentUserId() async {
    return await _storage.read(key: 'user_id');
  }

  static Future<String?> getAccessToken() async {
    return await _storage.read(key: 'access_token'); // SecureStorage
  }
}
