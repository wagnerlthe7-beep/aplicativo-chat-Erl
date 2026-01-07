import 'package:flutter/material.dart';
import 'package:permission_handler/permission_handler.dart';
import 'permission_service.dart';
import 'app_theme.dart';

class PermissionsPage extends StatefulWidget {
  @override
  _PermissionsPageState createState() => _PermissionsPageState();
}

class _PermissionsPageState extends State<PermissionsPage> {
  Map<Permission, bool> _permissionStatuses = {};
  bool _isLoading = false;
  int _currentPermissionIndex = 0;

  final List<Permission> _permissions =
      PermissionService.getEssentialPermissions();

  @override
  void initState() {
    super.initState();
    _checkAllPermissions();
  }

  Future<void> _checkAllPermissions() async {
    final statuses = await PermissionService.checkMultiplePermissions(
      _permissions,
    );
    setState(() {
      _permissionStatuses = statuses;
    });
  }

  Future<void> _requestPermission(Permission permission) async {
    setState(() => _isLoading = true);

    final granted = await PermissionService.requestPermission(permission);

    setState(() {
      _permissionStatuses[permission] = granted;
      _isLoading = false;
    });

    if (granted) {
      _showPermissionGrantedSnackBar(permission);
    } else {
      _showPermissionDeniedDialog(permission);
    }
  }

  Future<void> _requestAllRemainingPermissions() async {
    setState(() => _isLoading = true);

    final deniedPermissions = _permissionStatuses.entries
        .where((entry) => !entry.value)
        .map((entry) => entry.key)
        .toList();

    if (deniedPermissions.isNotEmpty) {
      final results = await PermissionService.requestMultiplePermissions(
        deniedPermissions,
      );
      setState(() {
        _permissionStatuses.addAll(results);
      });
    }

    setState(() => _isLoading = false);

    // Verificar se todas foram concedidas
    final allGranted = _permissionStatuses.values.every((granted) => granted);
    if (allGranted) {
      _navigateToChatList();
    }
  }

  void _navigateToChatList() {
    Navigator.pushReplacementNamed(context, '/chatList');
  }

  void _showPermissionGrantedSnackBar(Permission permission) {
    ScaffoldMessenger.of(context).showSnackBar(
      SnackBar(
        content: Text(
          '✅ ${PermissionService.getPermissionTitle(permission)} permitido!',
        ),
        backgroundColor: AppTheme.appBarColor,
        duration: Duration(seconds: 2),
      ),
    );
  }

  void _showPermissionDeniedDialog(Permission permission) {
    showDialog(
      context: context,
      builder: (context) => AlertDialog(
        title: Text('Permissão Necessária'),
        content: Text(
          'Para usar todas as funcionalidades do SpeekJoy, precisamos da permissão de ${PermissionService.getPermissionTitle(permission).toLowerCase()}. '
          'Você pode ativá-la nas configurações do dispositivo.',
        ),
        actions: [
          TextButton(
            onPressed: () => Navigator.pop(context),
            child: Text('Cancelar'),
          ),
          ElevatedButton(
            onPressed: () {
              Navigator.pop(context);
              openAppSettings();
            },
            child: Text('Configurações'),
            style: ElevatedButton.styleFrom(backgroundColor: AppTheme.appBarColor),
          ),
        ],
      ),
    );
  }

  @override
  Widget build(BuildContext context) {
    final allGranted = _permissionStatuses.values.every((granted) => granted);
    final someGranted = _permissionStatuses.values.any((granted) => granted);

    return Scaffold(
      backgroundColor: Colors.white,
      appBar: AppBar(
        backgroundColor: Colors.white,
        elevation: 0,
        automaticallyImplyLeading: false,
        title: Text(
          'Permissões',
          style: TextStyle(
            color: Colors.grey[800],
            fontSize: 18,
            fontWeight: FontWeight.w500,
          ),
        ),
      ),
      body: SafeArea(
        child: Padding(
          padding: EdgeInsets.all(24),
          child: Column(
            crossAxisAlignment: CrossAxisAlignment.stretch,
            children: [
              SizedBox(height: 20),
              Icon(Icons.security, size: 80, color: AppTheme.appBarColor),
              SizedBox(height: 30),
              Text(
                'Permissões Necessárias',
                style: TextStyle(
                  fontSize: 24,
                  fontWeight: FontWeight.bold,
                  color: Colors.grey[800],
                ),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 16),
              Text(
                'Para uma melhor experiência, o SpeekJoy precisa de algumas permissões para funcionar completamente.',
                style: TextStyle(fontSize: 16, color: Colors.grey[600]),
                textAlign: TextAlign.center,
              ),
              SizedBox(height: 40),

              // Lista de permissões
              Expanded(
                child: ListView.builder(
                  itemCount: _permissions.length,
                  itemBuilder: (context, index) {
                    final permission = _permissions[index];
                    final isGranted = _permissionStatuses[permission] ?? false;

                    return Container(
                      margin: EdgeInsets.only(bottom: 16),
                      decoration: BoxDecoration(
                        border: Border.all(
                          color: isGranted ? AppTheme.appBarColor : Colors.grey[300]!,
                          width: 1,
                        ),
                        borderRadius: BorderRadius.circular(12),
                        color: isGranted ? AppTheme.appBarColor.withOpacity(0.1) : Colors.white,
                      ),
                      child: ListTile(
                        leading: Icon(
                          PermissionService.getPermissionIcon(permission),
                          color: isGranted ? AppTheme.appBarColor : Colors.grey[600],
                          size: 28,
                        ),
                        title: Text(
                          PermissionService.getPermissionTitle(permission),
                          style: TextStyle(
                            fontWeight: FontWeight.w600,
                            color: isGranted
                                ? AppTheme.appBarColor
                                : Colors.grey[800],
                          ),
                        ),
                        subtitle: Text(
                          PermissionService.getPermissionDescription(
                            permission,
                          ),
                          style: TextStyle(
                            fontSize: 12,
                            color: Colors.grey[600],
                          ),
                        ),
                        trailing: isGranted
                            ? Icon(
                                Icons.check_circle,
                                color: AppTheme.appBarColor,
                                size: 24,
                              )
                            : IconButton(
                                icon: Icon(
                                  Icons.arrow_forward_ios,
                                  color: Colors.grey[400],
                                ),
                                onPressed: _isLoading
                                    ? null
                                    : () => _requestPermission(permission),
                              ),
                        onTap: _isLoading || isGranted
                            ? null
                            : () => _requestPermission(permission),
                      ),
                    );
                  },
                ),
              ),

              SizedBox(height: 20),

              // Botões de ação
              if (!allGranted) ...[
                ElevatedButton(
                  onPressed: _isLoading
                      ? null
                      : _requestAllRemainingPermissions,
                  style: ElevatedButton.styleFrom(
                    backgroundColor: AppTheme.appBarColor,
                    foregroundColor: AppTheme.textOnGreen,
                    padding: EdgeInsets.symmetric(vertical: 16),
                    shape: RoundedRectangleBorder(
                      borderRadius: BorderRadius.circular(12),
                    ),
                    elevation: 0,
                  ),
                  child: _isLoading
                      ? Row(
                          mainAxisAlignment: MainAxisAlignment.center,
                          children: [
                            SizedBox(
                              width: 20,
                              height: 20,
                              child: CircularProgressIndicator(
                                strokeWidth: 2,
                                valueColor: AlwaysStoppedAnimation<Color>(
                                  Colors.white,
                                ),
                              ),
                            ),
                            SizedBox(width: 12),
                            Text('Solicitando...'),
                          ],
                        )
                      : Text(
                          'Permitir Todas',
                          style: TextStyle(
                            fontSize: 16,
                            fontWeight: FontWeight.bold,
                          ),
                        ),
                ),
                SizedBox(height: 12),
              ],

              TextButton(
                onPressed: _navigateToChatList,
                child: Text(
                  allGranted ? 'Continuar' : 'Pular por agora',
                  style: TextStyle(
                    fontSize: 16,
                    color: allGranted ? AppTheme.appBarColor : Colors.grey[600],
                    fontWeight: FontWeight.w500,
                  ),
                ),
              ),

              SizedBox(height: 20),
              Text(
                'Você pode alterar essas permissões a qualquer momento nas configurações do dispositivo.',
                style: TextStyle(fontSize: 12, color: Colors.grey[500]),
                textAlign: TextAlign.center,
              ),
            ],
          ),
        ),
      ),
    );
  }
}
