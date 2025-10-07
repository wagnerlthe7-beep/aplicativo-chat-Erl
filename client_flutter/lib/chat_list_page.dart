import 'package:flutter/material.dart';
import 'package:flutter_secure_storage/flutter_secure_storage.dart';

class ChatListPage extends StatelessWidget {
  final storage = FlutterSecureStorage();

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
              child: Text('Logout & Reset Token'),
              onPressed: () async {
                // Remove o token salvo
                await storage.delete(key: 'api_token');

                // Opcional: remove tudo do storage
                // await storage.deleteAll();

                // Volta para a tela inicial
                Navigator.pushNamedAndRemoveUntil(
                  context,
                  '/startup', // substitua pela sua rota inicial
                  (route) => false,
                );
              },
            ),
          ],
        ),
      ),
    );
  }
}
