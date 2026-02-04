import 'package:flutter/material.dart';
import 'auth_service.dart';
import 'app_theme.dart';

class NameInputPage extends StatefulWidget {
  final String phoneNumber;
  
  const NameInputPage({Key? key, required this.phoneNumber}) : super(key: key);

  @override
  _NameInputPageState createState() => _NameInputPageState();
}

class _NameInputPageState extends State<NameInputPage> {
  final _nameController = TextEditingController();
  bool _loading = false;

  void _submitName() async {
    final name = _nameController.text.trim();
    if (name.isEmpty) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Por favor, digite seu nome')),
      );
      return;
    }

    setState(() => _loading = true);

    try {
      // Enviar nome para o backend junto com a autenticação
      final success = await AuthService.completeRegistrationWithName(name);
      
      if (success) {
        // Navegar para a página de permissões
        Navigator.pushReplacementNamed(context, '/permissions');
      } else {
        ScaffoldMessenger.of(context).showSnackBar(
          SnackBar(content: Text('Erro ao finalizar cadastro')),
        );
      }
    } catch (e) {
      ScaffoldMessenger.of(context).showSnackBar(
        SnackBar(content: Text('Erro: $e')),
      );
    } finally {
      setState(() => _loading = false);
    }
  }

  @override
  Widget build(BuildContext context) {
    return Scaffold(
      appBar: AppBar(
        title: Text('Seu Nome'),
        automaticallyImplyLeading: false, // Remove o botão de voltar
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
                Icons.person_outline,
                size: 80,
                color: AppTheme.appBarColor,
              ),
              SizedBox(height: 30),
              Text(
                'Como você gostaria de ser chamado?',
                style: TextStyle(
                  fontSize: 24,
                  fontWeight: FontWeight.bold,
                  color: Colors.grey[800],
                ),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 16),
              Text(
                'Escolha um nome que seus contatos reconhecerão',
                style: TextStyle(
                  fontSize: 16,
                  color: Colors.grey[600],
                ),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 40),
              TextField(
                controller: _nameController,
                decoration: InputDecoration(
                  labelText: 'Seu nome',
                  hintText: 'Digite seu nome completo',
                  border: OutlineInputBorder(
                    borderRadius: BorderRadius.circular(12),
                  ),
                  prefixIcon: Icon(Icons.person),
                ),
                textCapitalization: TextCapitalization.words,
                maxLength: 50,
                enabled: !_loading,
              ),
              SizedBox(height: 30),
              ElevatedButton(
                onPressed: _loading ? null : _submitName,
                style: ElevatedButton.styleFrom(
                  backgroundColor: AppTheme.appBarColor,
                  foregroundColor: AppTheme.textOnGreen,
                  padding: EdgeInsets.symmetric(vertical: 16),
                  shape: RoundedRectangleBorder(
                    borderRadius: BorderRadius.circular(12),
                  ),
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
                          Text('Finalizando...'),
                        ],
                      )
                    : Text(
                        'Continuar',
                        style: TextStyle(
                          fontSize: 16,
                          fontWeight: FontWeight.bold,
                        ),
                      ),
              ),
            ],
            ),
          ),
        ),
      ),
    );
  }

  @override
  void dispose() {
    _nameController.dispose();
    super.dispose();
  }
}
