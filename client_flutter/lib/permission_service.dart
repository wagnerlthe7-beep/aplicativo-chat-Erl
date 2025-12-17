import 'package:permission_handler/permission_handler.dart';
import 'package:flutter/material.dart';

class PermissionService {
  static final Map<Permission, String> _permissionDescriptions = {
    Permission.camera: 'Permitir acesso à câmera para tirar fotos e vídeos',
    Permission.storage: 'Permitir acesso ao armazenamento para salvar e compartilhar arquivos',
    Permission.photos: 'Permitir acesso às fotos para compartilhar imagens',
    Permission.contacts: 'Permitir acesso aos contatos para encontrar amigos',
    Permission.microphone: 'Permitir acesso ao microfone para gravações de voz',
  };

  static final Map<Permission, IconData> _permissionIcons = {
    Permission.camera: Icons.camera_alt,
    Permission.storage: Icons.folder,
    Permission.photos: Icons.photo_library,
    Permission.contacts: Icons.contacts,
    Permission.microphone: Icons.mic,
  };

  static final Map<Permission, String> _permissionTitles = {
    Permission.camera: 'Câmera',
    Permission.storage: 'Armazenamento',
    Permission.photos: 'Galeria',
    Permission.contacts: 'Contatos',
    Permission.microphone: 'Microfone',
  };

  /// Verifica se uma permissão está concedida
  static Future<bool> isGranted(Permission permission) async {
    final status = await permission.status;
    return status == PermissionStatus.granted;
  }

  /// Solicita uma permissão específica
  static Future<bool> requestPermission(Permission permission) async {
    final status = await permission.request();
    return status == PermissionStatus.granted;
  }

  /// Solicita múltiplas permissões
  static Future<Map<Permission, bool>> requestMultiplePermissions(List<Permission> permissions) async {
    final Map<Permission, PermissionStatus> statuses = await permissions.request();
    
    return Map.fromEntries(
      statuses.entries.map((entry) => MapEntry(entry.key, entry.value == PermissionStatus.granted))
    );
  }

  /// Verifica o status de múltiplas permissões
  static Future<Map<Permission, bool>> checkMultiplePermissions(List<Permission> permissions) async {
    final Map<Permission, bool> results = {};
    
    for (Permission permission in permissions) {
      results[permission] = await isGranted(permission);
    }
    
    return results;
  }

  /// Abre as configurações do app se a permissão foi negada permanentemente
  static Future<void> openAppSettings() async {
    await openAppSettings();
  }

  /// Retorna a descrição de uma permissão
  static String getPermissionDescription(Permission permission) {
    return _permissionDescriptions[permission] ?? 'Permissão necessária';
  }

  /// Retorna o ícone de uma permissão
  static IconData getPermissionIcon(Permission permission) {
    return _permissionIcons[permission] ?? Icons.security;
  }

  /// Retorna o título de uma permissão
  static String getPermissionTitle(Permission permission) {
    return _permissionTitles[permission] ?? 'Permissão';
  }

  /// Lista de permissões essenciais para o app
  static List<Permission> getEssentialPermissions() {
    return [
      Permission.camera,
      Permission.storage,
      Permission.photos,
      Permission.contacts,
      Permission.microphone,
    ];
  }

  /// Verifica se todas as permissões essenciais foram concedidas
  static Future<bool> areAllEssentialPermissionsGranted() async {
    final permissions = getEssentialPermissions();
    final results = await checkMultiplePermissions(permissions);
    
    return results.values.every((isGranted) => isGranted);
  }

  /// Solicita todas as permissões essenciais
  static Future<Map<Permission, bool>> requestAllEssentialPermissions() async {
    final permissions = getEssentialPermissions();
    return await requestMultiplePermissions(permissions);
  }
}
