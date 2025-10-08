import 'package:flutter/material.dart';
import 'auth_service.dart';

class OtpPage extends StatefulWidget {
  @override
  _OtpPageState createState() => _OtpPageState();
}

class _OtpPageState extends State<OtpPage> {
  final _codeController = TextEditingController();
  bool _loading = false;
  String? verificationId;

  @override
  void didChangeDependencies() {
    super.didChangeDependencies();
    verificationId ??= ModalRoute.of(context)!.settings.arguments as String?;
  }

  void _submitCode() async {
    final code = _codeController.text.trim();
    if (code.isEmpty || verificationId == null) return;

    setState(() => _loading = true);

    // Login no Firebase usando o código SMS
    final success = await AuthService.signInWithSmsCode(
      verificationId: verificationId!,
      smsCode: code,
    );

    if (success) {
      // AuthService.signInWithSmsCode já chama afterFirebaseSignInBackend automaticamente
      // Então só navega para a próxima tela
      Navigator.pushReplacementNamed(context, '/chatList');
    } else {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Código inválido')),
      );
    }

    setState(() => _loading = false);
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Código SMS')),
      body: Padding(
        padding: EdgeInsets.all(20),
        child: Column(
          children: [
            Text('Digite o código que recebeu por SMS:'),
            TextField(
              controller: _codeController,
              keyboardType: TextInputType.number,
            ),
            SizedBox(height: 20),
            _loading
                ? CircularProgressIndicator()
                : ElevatedButton(
                    onPressed: _submitCode,
                    child: Text('Confirmar'),
                  ),
          ],
        ),
      ),
    );
  }
}
