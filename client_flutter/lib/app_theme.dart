import 'package:flutter/material.dart';

/// Tema do aplicativo baseado no design da imagem fornecida
/// Cores principais: Verde WhatsApp-like com tons suaves
class AppTheme {
  // Cores principais
  static const Color primaryGreen = Color(0xFF075E54); // Verde escuro principal
  static const Color lightGreen = Color(0xFF128C7E); // Verde médio
  static const Color accentGreen = Color(0xFF25D366); // Verde claro/accent
  static const Color tealGreen = Color(
    0xFF34B7F1,
  ); // Azul-esverdeado para status

  // Cores de fundo
  static const Color backgroundColor = Color(
    0xFFECE5DD,
  ); // Bege claro (fundo do chat)
  static const Color surfaceColor = Color(
    0xFFFFFFFF,
  ); // Branco para cards/superfícies
  static const Color appBarColor = primaryGreen; // Cor do AppBar

  // Cores de texto
  static const Color textPrimary = Color(0xFF000000);
  static const Color textSecondary = Color(0xFF757575);
  static const Color textLight = Color(0xFF9E9E9E);
  static const Color textOnGreen = Color(0xFFFFFFFF); // Texto sobre verde

  // Cores de mensagens
  static const Color messageSent =
      accentGreen; // Mensagens enviadas (verde claro)
  static const Color messageReceived = Color(
    0xFFFFFFFF,
  ); // Mensagens recebidas (branco)
  static const Color messageReceivedText = Color(
    0xFF000000,
  ); // Texto mensagens recebidas
  static const Color messageSentText = Color(
    0xFFFFFFFF,
  ); // Texto mensagens enviadas

  // Cores de status
  static const Color statusRead = tealGreen; // Duplo check azul (lido)
  static const Color statusDelivered = Color(
    0xFF9E9E9E,
  ); // Duplo check cinza (entregue)
  static const Color statusSent = Color(0xFF9E9E9E); // Check simples (enviado)

  // Cores de elementos UI
  static const Color dividerColor = Color(0xFFE0E0E0);
  static const Color inputBackground = Color(0xFFFFFFFF);
  static const Color inputBorder = Color(0xFFE0E0E0);
  static const Color searchBackground = Color(0xFFF5F5F5);
  static const Color unreadBadge = accentGreen; // Badge de não lidas

  // Cores de ações
  static const Color actionReply = accentGreen;
  static const Color actionEdit = Color(0xFF2196F3); // Azul para editar
  static const Color actionDelete = Color(0xFFF44336); // Vermelho para deletar
  static const Color actionCopy = Color(0xFF757575); // Cinza para copiar

  // Cores de preview de reply
  static const Color replyPreviewBackground = Color(0xFFF5F5F5);
  static const Color replyPreviewBorder = accentGreen;
  static const Color replyPreviewText = Color(0xFF757575);

  // Cores de avatar
  static const Color avatarBackground = Color(0xFFE0E0E0);
  static const Color avatarIcon = primaryGreen;

  // Cores de presença
  static const Color presenceOnline = Color(0xFF4CAF50); // Verde para online
  static const Color presenceOffline = Color(0xFF9E9E9E); // Cinza para offline

  // Métodos auxiliares para opacidade
  static Color withOpacity(Color color, double opacity) {
    return color.withOpacity(opacity);
  }

  // Tema completo do Material Design
  static ThemeData get lightTheme {
    return ThemeData(
      primaryColor: primaryGreen,
      scaffoldBackgroundColor: backgroundColor,
      appBarTheme: const AppBarTheme(
        backgroundColor: appBarColor,
        elevation: 0,
        iconTheme: IconThemeData(color: textOnGreen),
        titleTextStyle: TextStyle(
          color: textOnGreen,
          fontSize: 20,
          fontWeight: FontWeight.w500,
        ),
      ),
      colorScheme: ColorScheme.light(
        primary: primaryGreen,
        secondary: accentGreen,
        surface: surfaceColor,
        background: backgroundColor,
        onPrimary: textOnGreen,
        onSecondary: textOnGreen,
        onSurface: textPrimary,
        onBackground: textPrimary,
      ),
      cardTheme: CardThemeData(color: surfaceColor, elevation: 0),
      dividerTheme: const DividerThemeData(color: dividerColor, thickness: 0.5),
      inputDecorationTheme: InputDecorationTheme(
        filled: true,
        fillColor: inputBackground,
        border: OutlineInputBorder(
          borderRadius: BorderRadius.circular(25),
          borderSide: const BorderSide(color: inputBorder),
        ),
        enabledBorder: OutlineInputBorder(
          borderRadius: BorderRadius.circular(25),
          borderSide: const BorderSide(color: inputBorder),
        ),
        focusedBorder: OutlineInputBorder(
          borderRadius: BorderRadius.circular(25),
          borderSide: const BorderSide(color: searchBackground, width: 2),
        ),
      ),
    );
  }
}
