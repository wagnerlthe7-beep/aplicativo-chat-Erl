import 'package:flutter/material.dart';
import 'app_theme.dart';

/// Splash Screen - Mostra ícone da app enquanto carrega
class SplashScreen extends StatelessWidget {
  @override
  Widget build(BuildContext context) {
    return Scaffold(
      backgroundColor: Colors.white,
      body: Center(
        child: Column(
          mainAxisAlignment: MainAxisAlignment.center,
          children: [
            // ✅ Ícone da app (pode usar logo se tiver)
            Container(
              width: 120,
              height: 120,
              decoration: BoxDecoration(
                color: AppTheme.appBarColor,
                shape: BoxShape.circle,
              ),
              child: Icon(
                Icons.chat_bubble_outline,
                size: 80,
                color: AppTheme.textOnGreen,
              ),
            ),
            SizedBox(height: 24),
            // ✅ Nome da app
            Text(
              'SpeekJoy',
              style: TextStyle(
                fontSize: 28,
                fontWeight: FontWeight.bold,
                color: AppTheme.appBarColor,
              ),
            ),
            SizedBox(height: 8),
            // ✅ Indicador de carregamento
            SizedBox(
              width: 40,
              height: 40,
              child: CircularProgressIndicator(
                strokeWidth: 3,
                valueColor: AlwaysStoppedAnimation<Color>(
                  AppTheme.appBarColor,
                ),
              ),
            ),
          ],
        ),
      ),
    );
  }
}

