import 'package:flutter/material.dart';

class WelcomePage extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      body: SafeArea(
        child: Center(
          child: Padding(
            padding: EdgeInsets.symmetric(horizontal: 24),
            child: Column(
              mainAxisAlignment: MainAxisAlignment.center,
              children: [
                Icon(Icons.chat_bubble, size: 100, color: Colors.green),
                SizedBox(height: 20),
                Text(
                  "Bem-vindo ao WhaClone",
                  style: TextStyle(fontSize: 24, fontWeight: FontWeight.bold),
                ),
                SizedBox(height: 40),
                ElevatedButton(
                  onPressed: () =>
                      Navigator.pushReplacementNamed(context, '/phone'),
                  child: Text("Concordar e Continuar"),
                ),
              ],
            ),
          ),
        ),
      ),
    );
  }
}
