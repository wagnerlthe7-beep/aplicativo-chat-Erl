import 'package:flutter/material.dart';
import 'auth_service.dart';
import 'app_theme.dart';

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
      // ✅✅✅ NOVO: Verificar se é um usuário novo ou existente
      final isNewUser = await AuthService.isNewUser();
      
      if (isNewUser) {
        // Usuário novo: navegar para página de nome
        Navigator.pushReplacementNamed(context, '/nameInput');
      } else {
        // Usuário existente: finalizar login normalmente
        await AuthService.completeExistingUserLogin();
        Navigator.pushReplacementNamed(context, '/chatList');
      }
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
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: Colors.white,
        elevation: 0,
        leading: IconButton(
          icon: Icon(Icons.arrow_back, color: AppTheme.appBarColor),
          onPressed: () => Navigator.pop(context),
        ),
        title: Text(
          'Código SMS',
          style: TextStyle(
            color: Colors.grey[800],
            fontSize: 18,
            fontWeight: FontWeight.w500,
          ),
        ),
      ),
      body: SafeArea(
        child: SingleChildScrollView(
          child: Padding(
            padding: EdgeInsets.all(24),
            child: Column(
              crossAxisAlignment: CrossAxisAlignment.stretch,
              children: [
              SizedBox(height: 40),
              Icon(
                Icons.sms,
                size: 80,
                color: AppTheme.appBarColor,
              ),
              SizedBox(height: 30),
              Text(
                'Digite o código de verificação',
                style: TextStyle(
                  fontSize: 24,
                  fontWeight: FontWeight.bold,
                  color: Colors.grey[800],
                ),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 16),
              Text(
                'Enviamos um código de 6 dígitos para o seu número',
                style: TextStyle(
                  fontSize: 16,
                  color: Colors.grey[600],
                ),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 50),
              TextField(
                controller: _codeController,
                keyboardType: TextInputType.number,
                enabled: !_loading,
                textAlign: TextAlign.center,
                style: TextStyle(
                  fontSize: 24,
                  fontWeight: FontWeight.bold,
                  letterSpacing: 8,
                ),
                maxLength: 6,
                decoration: InputDecoration(
                  labelText: 'Código de verificação',
                  hintText: '000000',
                  prefixIcon: Icon(Icons.lock, color: AppTheme.appBarColor),
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(12),
                    borderSide: BorderSide(color: Colors.grey[300]!),
                  ),
                  focusedBorder: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(12),
                    borderSide: BorderSide(color: AppTheme.appBarColor, width: 2),
                  ),
                  enabledBorder: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(12),
                    borderSide: BorderSide(color: Colors.grey[300]!),
                  ),
                  contentPadding: EdgeInsets.symmetric(horizontal: 16, vertical: 16),
                  counterText: '', // Remove o contador de caracteres
                ),
              ),
              SizedBox(height: 40),
              ElevatedButton(
                onPressed: _loading ? null : _submitCode,
                style: ElevatedButton.styleFrom(
                  backgroundColor: AppTheme.appBarColor,
                  foregroundColor: AppTheme.textOnGreen,
                  padding: EdgeInsets.symmetric(vertical: 16),
                  shape: RoundedRectangleBorder(
                    borderRadius: BorderRadius.circular(12),
                  ),
                  elevation: 0,
                ),
                child: _loading
                    ? Row(
                        mainAxisAlignment: MainAxisAlignment.center,
                        children: [
                          SizedBox(
                            width: 20,
                            height: 20,
                            child: CircularProgressIndicator(
                              strokeWidth: 2,
                              valueColor: AlwaysStoppedAnimation<Color>(Colors.white),
                            ),
                          ),
                          SizedBox(width: 12),
                          Text('Verificando...'),
                        ],
                      )
                    : Text(
                        'Verificar código',
                        style: TextStyle(
                          fontSize: 16,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
              ),
              SizedBox(height: 30),
              Row(
                mainAxisAlignment: MainAxisAlignment.center,
                children: [
                  Text(
                    'Não recebeu o código? ',
                    style: TextStyle(
                      fontSize: 14,
                      color: Colors.grey[600],
                    ),
                  ),
                  GestureDetector(
                    onTap: () {
                      // TODO: Implementar reenvio de código
                      ScaffoldMessenger.of(context).showSnackBar(
                        SnackBar(content: Text('Funcionalidade em desenvolvimento')),
                      );
                    },
                    child: Text(
                      'Reenviar',
                      style: TextStyle(
                        fontSize: 14,
                        color: AppTheme.appBarColor,
                        fontWeight: FontWeight.w500,
                      ),
                    ),
                  ),
                ],
              ),
              SizedBox(height: 20),
              ],
            ),
          ),
        ),
      ),
    );
  }

  @override
  void dispose() {
    _codeController.dispose();
    super.dispose();
  }
}
