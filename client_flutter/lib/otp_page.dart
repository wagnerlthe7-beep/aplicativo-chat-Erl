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

    final success = await AuthService.signInWithSmsCode(
      verificationId: verificationId!,
      smsCode: code,
    );
    setState(() => _loading = false);
    // Imprime o token no console do Flutter
    await AuthService.printFirebaseIdToken();
    if (success) {
      Navigator.pushReplacementNamed(context, '/chatList');
    } else {
      ScaffoldMessenger.of(
        context,
      ).showSnackBar(SnackBar(content: Text('Código inválido')));
    }
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
