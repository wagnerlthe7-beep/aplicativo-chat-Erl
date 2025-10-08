import 'package:flutter/material.dart';
import 'auth_service.dart';

class ChatListPage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(title: Text('Conversas')),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Text('Aqui vai a lista de conversas'),
            SizedBox(height: 20),
            ElevatedButton(
              child: Text('Logout & Reset Tokens'),
              onPressed: () async {
                try {
                  // Chama logout do backend e limpa storage
                  await AuthService.logout();

                  // Volta para a tela inicial / startup
                  Navigator.pushNamedAndRemoveUntil(
                    context,
                    '/', // substitua pela tua rota inicial
                    (route) => false,
                  );
                } catch (e) {
                  // Mostra erro caso algo falhe
                  ScaffoldMessenger.of(context).showSnackBar(
                    SnackBar(content: Text('Erro ao fazer logout: $e')),
                  );
                }
              },
            ),
          ],
        ),
      ),
    );
  }
}
