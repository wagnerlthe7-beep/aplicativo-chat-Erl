import 'package:flutter/material.dart';
import 'splash_screen.dart';
import 'chat_list_page.dart';

/// Wrapper que mostra SplashScreen e depois navega para a tela de destino
/// ✅ Só mostra splash quando app é aberto do zero (cold start), não quando volta do background
class SplashScreenWrapper extends StatefulWidget {
  final String targetRoute;
  final bool isColdStart; // ✅ Flag indicando se é cold start

  const SplashScreenWrapper({
    Key? key,
    required this.targetRoute,
    this.isColdStart = true,
  }) : super(key: key);

  @override
  _SplashScreenWrapperState createState() => _SplashScreenWrapperState();
}

class _SplashScreenWrapperState extends State<SplashScreenWrapper>
    with WidgetsBindingObserver {
  bool _shouldShowSplash = false;
  bool _hasNavigated = false;

  @override
  void initState() {
    super.initState();
    WidgetsBinding.instance.addObserver(this);

    // ✅ Só mostrar splash se for cold start (app foi aberto do zero)
    if (widget.isColdStart) {
      _shouldShowSplash = true;

      // ✅ Mostrar splash por um tempo mínimo (1 segundo) e depois navegar
      Future.delayed(Duration(milliseconds: 1000), () {
        if (mounted && !_hasNavigated) {
          _hasNavigated = true;
          Navigator.pushReplacement(
            context,
            MaterialPageRoute(
              builder: (context) {
                switch (widget.targetRoute) {
                  case '/chatList':
                    return ChatListPage();
                  default:
                    return ChatListPage();
                }
              },
            ),
          );
        }
      });
    } else {
      // ✅ App estava em background - ir direto para a tela de destino (sem splash)
      WidgetsBinding.instance.addPostFrameCallback((_) {
        if (mounted && !_hasNavigated) {
          _hasNavigated = true;
          Navigator.pushReplacement(
            context,
            MaterialPageRoute(
              builder: (context) {
                switch (widget.targetRoute) {
                  case '/chatList':
                    return ChatListPage();
                  default:
                    return ChatListPage();
                }
              },
            ),
          );
        }
      });
    }
  }

  @override
  void dispose() {
    WidgetsBinding.instance.removeObserver(this);
    super.dispose();
  }

  @override
  Widget build(BuildContext context) {
    // ✅ Só mostrar splash se for cold start
    if (_shouldShowSplash) {
      return SplashScreen();
    } else {
      // ✅ Se não deve mostrar splash, mostrar tela de destino diretamente
      // (será substituído pelo Navigator.pushReplacement no postFrameCallback)
      return Scaffold(
        backgroundColor: Colors.white,
        body: Center(child: CircularProgressIndicator()),
      );
    }
  }
}
