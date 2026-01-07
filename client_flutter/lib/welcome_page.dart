import 'package:flutter/material.dart';
import 'app_theme.dart';

class WelcomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: SafeArea(
        child: Padding(
          padding: EdgeInsets.all(24),
          child: Column(
            children: [
              Expanded(
                child: Column(
                  mainAxisAlignment: MainAxisAlignment.center,
                  children: [
                    SizedBox(height: 60),
                    Icon(
                      Icons.chat_bubble_outline,
                      size: 120,
                      color: AppTheme.appBarColor,
                    ),
                    SizedBox(height: 40),
                    Text(
                      "Bem-vindo ao SpeekJoy",
                      style: TextStyle(
                        fontSize: 28,
                        fontWeight: FontWeight.bold,
                        color: Colors.grey[800],
                      ),
                      textAlign: TextAlign.center,
                    ),
                    SizedBox(height: 16),
                    Text(
                      "Conecte-se com amigos e família",
                      style: TextStyle(fontSize: 16, color: Colors.grey[600]),
                      textAlign: TextAlign.center,
                    ),
                    SizedBox(height: 60),
                  ],
                ),
              ),
              Column(
                children: [
                  Text(
                    "Ao tocar em \"Aceitar e Continuar\", você concorda com os",
                    style: TextStyle(fontSize: 12, color: Colors.grey[600]),
                    textAlign: TextAlign.center,
                  ),
                  SizedBox(height: 4),
                  Row(
                    mainAxisAlignment: MainAxisAlignment.center,
                    children: [
                      GestureDetector(
                        onTap: () {
                          // TODO: Mostrar termos de uso
                          _showTermsDialog(context);
                        },
                        child: Text(
                          "Termos de Serviço",
                          style: TextStyle(
                            fontSize: 12,
                            color: AppTheme.appBarColor,
                            decoration: TextDecoration.underline,
                          ),
                        ),
                      ),
                      Text(
                        " e ",
                        style: TextStyle(fontSize: 12, color: Colors.grey[600]),
                      ),
                      GestureDetector(
                        onTap: () {
                          // TODO: Mostrar política de privacidade
                          _showPrivacyDialog(context);
                        },
                        child: Text(
                          "Política de Privacidade",
                          style: TextStyle(
                            fontSize: 12,
                            color: AppTheme.appBarColor,
                            decoration: TextDecoration.underline,
                          ),
                        ),
                      ),
                    ],
                  ),
                  SizedBox(height: 30),
                  SizedBox(
                    width: double.infinity,
                    child: ElevatedButton(
                      onPressed: () =>
                          Navigator.pushReplacementNamed(context, '/phone'),
                      style: ElevatedButton.styleFrom(
                        backgroundColor: AppTheme.appBarColor,
                        foregroundColor: AppTheme.textOnGreen,
                        padding: EdgeInsets.symmetric(vertical: 16),
                        shape: RoundedRectangleBorder(
                          borderRadius: BorderRadius.circular(12),
                        ),
                        elevation: 0,
                      ),
                      child: Text(
                        "Aceitar e Continuar",
                        style: TextStyle(
                          fontSize: 16,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
                    ),
                  ),
                  SizedBox(height: 20),
                ],
              ),
            ],
          ),
        ),
      ),
    );
  }

  void _showTermsDialog(BuildContext context) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Termos de Serviço'),
        content: SingleChildScrollView(
          child: Text(
            'Ao usar o SpeekJoy, você concorda em:\n\n'
            '• Respeitar outros usuários\n'
            '• Não enviar conteúdo inadequado\n'
            '• Manter suas informações atualizadas\n'
            '• Usar o serviço de forma responsável\n\n'
            'Violações podem resultar na suspensão da conta.',
            style: TextStyle(fontSize: 14),
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Fechar'),
          ),
        ],
      ),
    );
  }

  void _showPrivacyDialog(BuildContext context) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Política de Privacidade'),
        content: SingleChildScrollView(
          child: Text(
            'Sua privacidade é importante para nós:\n\n'
            '• Coletamos apenas informações necessárias\n'
            '• Seus dados são protegidos com criptografia\n'
            '• Não compartilhamos informações com terceiros\n'
            '• Você pode excluir sua conta a qualquer momento\n\n'
            'Para mais detalhes, entre em contato conosco.',
            style: TextStyle(fontSize: 14),
          ),
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Fechar'),
          ),
        ],
      ),
    );
  }
}
