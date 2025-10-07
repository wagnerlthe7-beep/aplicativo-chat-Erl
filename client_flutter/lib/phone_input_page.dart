import 'package:flutter/material.dart';
import 'auth_service.dart'; // veremos em seguida

class PhoneInputPage extends StatefulWidget {
  @override
  _PhoneInputPageState createState() => _PhoneInputPageState();
}

class _PhoneInputPageState extends State<PhoneInputPage> {
  final _controller = TextEditingController();
  bool _loading = false;

  void _sendCode() async {
    final phone = _controller.text.trim();
    if (phone.isEmpty) return;
    setState(() => _loading = true);

    // chama o AuthService que usa FirebaseAuth.verifyPhoneNumber
    await AuthService.verifyPhoneNumber(
      phoneNumber: phone,
      codeSent: (verificationId, resendToken) {
        setState(() => _loading = false);
        Navigator.pushReplacementNamed(
          context,
          '/otp',
          arguments: verificationId,
        );
      },
      verificationFailed: (e) {
        setState(() => _loading = false);
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Falha na verificação: ${e.message}')),
        );
      },
      autoRetrieved: (userCredential) {
        // auto sign-in case Android auto verifies
        _onSignInSuccess(userCredential);
      },
    );
  }

  void _onSignInSuccess(userCredential) {
    // aqui o AuthService já cuidou de trocar token com backend e salvar sessão,
    // então só navega para chatList
    Navigator.pushReplacementNamed(context, '/chatList');
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Verificar número')),
      body: Padding(
        padding: EdgeInsets.all(20),
        child: Column(
          children: [
            TextField(
              controller: _controller,
              keyboardType: TextInputType.phone,
              decoration: InputDecoration(
                hintText: '+258 84 123 4567',
                labelText: 'Número de telefone',
              ),
            ),
            SizedBox(height: 20),
            _loading
                ? CircularProgressIndicator()
                : ElevatedButton(
                    onPressed: _sendCode,
                    child: Text('Enviar código'),
                  ),
          ],
        ),
      ),
    );
  }
}
