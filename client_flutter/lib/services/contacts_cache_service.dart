// services/contacts_cache_service.dart
// Serviço de cache local para contatos registrados no app (estilo WhatsApp)
// Armazena quais contatos têm conta no SpeekJoy para funcionar offline

import 'package:sqflite/sqflite.dart';
import 'package:path/path.dart';

class ContactsCacheService {
  static Database? _database;
  static const String _tableName = 'contacts_cache';

  /// Inicializar database
  static Future<Database> get database async {
    if (_database != null) return _database!;
    _database = await _initDatabase();
    return _database!;
  }

  static Future<Database> _initDatabase() async {
    final dbPath = await getDatabasesPath();
    final path = join(dbPath, 'contacts_cache.db');

    return await openDatabase(
      path,
      version: 1,
      onCreate: (db, version) async {
        await db.execute('''
          CREATE TABLE $_tableName (
            phone TEXT PRIMARY KEY,
            user_id TEXT,
            name TEXT,
            is_registered INTEGER DEFAULT 0,
            last_sync INTEGER NOT NULL
          )
        ''');

        // Índices para busca rápida
        await db.execute('CREATE INDEX idx_phone ON $_tableName (phone)');
        await db.execute('CREATE INDEX idx_user_id ON $_tableName (user_id)');
        await db.execute('CREATE INDEX idx_is_registered ON $_tableName (is_registered)');

        print('✅ Tabela contacts_cache criada');
      },
    );
  }

  /// Salvar/atualizar contato no cache
  static Future<void> saveContact({
    required String phone,
    String? userId,
    String? name,
    required bool isRegistered,
  }) async {
    final db = await database;
    await db.insert(
      _tableName,
      {
        'phone': phone,
        'user_id': userId,
        'name': name,
        'is_registered': isRegistered ? 1 : 0,
        'last_sync': DateTime.now().millisecondsSinceEpoch,
      },
      conflictAlgorithm: ConflictAlgorithm.replace,
    );
  }

  /// Buscar contato no cache
  static Future<Map<String, dynamic>?> getContact(String phone) async {
    final db = await database;
    final results = await db.query(
      _tableName,
      where: 'phone = ?',
      whereArgs: [phone],
      limit: 1,
    );

    if (results.isEmpty) return null;

    final row = results.first;
    return {
      'phone': row['phone'] as String,
      'user_id': row['user_id'] as String?,
      'name': row['name'] as String?,
      'is_registered': (row['is_registered'] as int) == 1,
      'last_sync': row['last_sync'] as int,
    };
  }

  /// Verificar se contato está registrado (retorna null se não está no cache)
  static Future<bool?> isContactRegistered(String phone) async {
    final contact = await getContact(phone);
    return contact?['is_registered'] as bool?;
  }

  /// Buscar userId do contato (retorna null se não está registrado ou não está no cache)
  static Future<String?> getContactUserId(String phone) async {
    final contact = await getContact(phone);
    if (contact == null || contact['is_registered'] == false) {
      return null;
    }
    return contact['user_id'] as String?;
  }

  /// Salvar múltiplos contatos (batch insert)
  static Future<void> saveContacts(List<Map<String, dynamic>> contacts) async {
    final db = await database;
    final batch = db.batch();

    final now = DateTime.now().millisecondsSinceEpoch;

    for (final contact in contacts) {
      batch.insert(
        _tableName,
        {
          'phone': contact['phone'] as String,
          'user_id': contact['user_id'] as String?,
          'name': contact['name'] as String?,
          'is_registered': (contact['is_registered'] as bool) ? 1 : 0,
          'last_sync': now,
        },
        conflictAlgorithm: ConflictAlgorithm.replace,
      );
    }

    await batch.commit(noResult: true);
    print('✅ ${contacts.length} contatos salvos no cache');
  }

  /// Limpar cache antigo (opcional - limpar contatos não verificados há mais de X dias)
  static Future<void> clearOldCache({int daysOld = 30}) async {
    final db = await database;
    final cutoff = DateTime.now()
        .subtract(Duration(days: daysOld))
        .millisecondsSinceEpoch;

    final deleted = await db.delete(
      _tableName,
      where: 'last_sync < ? AND is_registered = 0',
      whereArgs: [cutoff],
    );

    print('🗑️ ${deleted} contatos antigos removidos do cache');
  }

  /// Limpar todo o cache
  static Future<void> clearAll() async {
    final db = await database;
    await db.delete(_tableName);
    print('🗑️ Cache de contatos limpo');
  }
}

