import 'package:flutter/material.dart';
import 'auth_service.dart';

class ChatListPage extends StatefulWidget {
  @override
  _ChatListPageState createState() => _ChatListPageState();
}

class _ChatListPageState extends State<ChatListPage> {
  bool _isLoading = false;

  Future<void> _logout() async {
    setState(() => _isLoading = true);
    try {
      await AuthService.logout();
      Navigator.pushNamedAndRemoveUntil(
        context,
        '/',
        (route) => false,
      );
    } catch (e) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Erro ao fazer logout: $e')),
      );
    } finally {
      setState(() => _isLoading = false);
    }
  }

  Future<void> _revokeOtherSessions() async {
    setState(() => _isLoading = true);
    try {
      final success = await AuthService.revokeOtherSessions();
      if (success) {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('✅ Outras sessões foram revogadas com sucesso!'),
            backgroundColor: Colors.green,
          ),
        );
      } else {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(
            content: Text('❌ Erro ao revogar outras sessões'),
            backgroundColor: Colors.red,
          ),
        );
      }
    } catch (e) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Erro: $e')),
      );
    } finally {
      setState(() => _isLoading = false);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Conversas'),
        actions: [
          PopupMenuButton<String>(
            onSelected: (value) {
              if (value == 'logout') {
                _logout();
              } else if (value == 'revoke_others') {
                _showRevokeDialog();
              }
            },
            itemBuilder: (context) => [
              PopupMenuItem(
                value: 'revoke_others',
                child: Row(
                  children: [
                    Icon(Icons.security, color: Colors.orange),
                    SizedBox(width: 8),
                    Text('Sair de outros dispositivos'),
                  ],
                ),
              ),
              PopupMenuItem(
                value: 'logout',
                child: Row(
                  children: [
                    Icon(Icons.logout, color: Colors.red),
                    SizedBox(width: 8),
                    Text('Logout'),
                  ],
                ),
              ),
            ],
          ),
        ],
      ),
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            Icon(
              Icons.chat_bubble_outline,
              size: 80,
              color: Colors.grey[400],
            ),
            SizedBox(height: 20),
            Text(
              'Aqui vai a lista de conversas',
              style: TextStyle(
                fontSize: 18,
                color: Colors.grey[600],
              ),
            ),
          ],
        ),
      ),
    );
  }

  void _showRevokeDialog() {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Sair de outros dispositivos'),
        content: Text(
          'Isso irá desconectar este dispositivo e todos os outros dispositivos que estão usando sua conta. '
          'Você precisará fazer login novamente em todos os dispositivos. Deseja continuar?',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Cancelar'),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              _revokeOtherSessions();
            },
            child: Text('Confirmar'),
            style: ElevatedButton.styleFrom(
              backgroundColor: Colors.orange,
              foregroundColor: Colors.white,
            ),
          ),
        ],
      ),
    );
  }
}
