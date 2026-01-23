// services/pending_messages_storage.dart
// Serviço de storage local para mensagens pendentes usando sqflite

import 'package:sqflite/sqflite.dart';
import 'package:path/path.dart';
import '../models/pending_message.dart';

class PendingMessagesStorage {
  static Database? _database;
  static const String _tableName = 'pending_messages';
  static const int _maxRetries = 5; // Máximo de tentativas antes de marcar como falha

  // ✅ Inicializar database
  static Future<Database> get database async {
    if (_database != null) return _database!;
    _database = await _initDatabase();
    return _database!;
  }

  static Future<Database> _initDatabase() async {
    final dbPath = await getDatabasesPath();
    final path = join(dbPath, 'pending_messages.db');

    return await openDatabase(
      path,
      version: 2, // ✅ Incrementar versão para adicionar novos campos
      onCreate: (db, version) async {
        // ✅ Criar tabela primeiro
        await db.execute('''
          CREATE TABLE $_tableName (
            msg_id TEXT PRIMARY KEY,
            to_user TEXT NOT NULL,
            from_user TEXT NOT NULL,
            content TEXT NOT NULL,
            status TEXT NOT NULL,
            created_at INTEGER NOT NULL,
            db_message_id TEXT,
            retry_count INTEGER DEFAULT 0,
            last_retry_at INTEGER,
            reply_to_id TEXT,
            reply_to_text TEXT,
            reply_to_sender_name TEXT,
            reply_to_sender_id TEXT,
            is_edited INTEGER DEFAULT 0,
            is_deleted INTEGER DEFAULT 0
          )
        ''');
        
        // ✅ Criar índices separadamente
        await db.execute('CREATE INDEX idx_status ON $_tableName (status)');
        await db.execute('CREATE INDEX idx_to_user ON $_tableName (to_user)');
        await db.execute('CREATE INDEX idx_created_at ON $_tableName (created_at)');
        
        print('✅ Tabela pending_messages criada com índices');
      },
      onUpgrade: (db, oldVersion, newVersion) async {
        // ✅ Migração: adicionar novos campos se versão antiga
        if (oldVersion < 2) {
          await db.execute('ALTER TABLE $_tableName ADD COLUMN reply_to_id TEXT');
          await db.execute('ALTER TABLE $_tableName ADD COLUMN reply_to_text TEXT');
          await db.execute('ALTER TABLE $_tableName ADD COLUMN reply_to_sender_name TEXT');
          await db.execute('ALTER TABLE $_tableName ADD COLUMN reply_to_sender_id TEXT');
          await db.execute('ALTER TABLE $_tableName ADD COLUMN is_edited INTEGER DEFAULT 0');
          await db.execute('ALTER TABLE $_tableName ADD COLUMN is_deleted INTEGER DEFAULT 0');
          print('✅ Tabela pending_messages atualizada para versão 2');
        }
      },
    );
  }

  // ✅ Salvar mensagem pendente
  static Future<void> savePendingMessage(PendingMessage message) async {
    final db = await database;
    await db.insert(
      _tableName,
      message.toMap(),
      conflictAlgorithm: ConflictAlgorithm.replace,
    );
    print('💾 Mensagem pendente salva: ${message.msgId} (status: ${message.status})');
  }

  // ✅ Buscar todas as mensagens pendentes
  static Future<List<PendingMessage>> getPendingMessages({
    String? status,
    String? toUserId,
    int? limit,
  }) async {
    final db = await database;
    
    String where = '1=1';
    List<dynamic> whereArgs = [];
    
    if (status != null) {
      where += ' AND status = ?';
      whereArgs.add(status);
    }
    
    if (toUserId != null) {
      where += ' AND to_user = ?';
      whereArgs.add(toUserId);
    }
    
    final orderBy = 'created_at ASC';
    
    final results = await db.query(
      _tableName,
      where: where,
      whereArgs: whereArgs.isNotEmpty ? whereArgs : null,
      orderBy: orderBy,
      limit: limit,
    );
    
    return results.map((map) => PendingMessage.fromMap(map)).toList();
  }

  // ✅ Buscar mensagens pending_local (prontas para sincronizar)
  static Future<List<PendingMessage>> getPendingLocalMessages() async {
    return getPendingMessages(status: 'pending_local');
  }

  // ✅ Atualizar status de uma mensagem
  static Future<void> updateMessageStatus(
    String msgId,
    String newStatus, {
    String? dbMessageId,
  }) async {
    final db = await database;
    final updates = <String, dynamic>{'status': newStatus};
    
    if (dbMessageId != null) {
      updates['db_message_id'] = dbMessageId;
    }
    
    await db.update(
      _tableName,
      updates,
      where: 'msg_id = ?',
      whereArgs: [msgId],
    );
    print('🔄 Status atualizado: $msgId -> $newStatus');
  }

  // ✅ Incrementar retry count
  static Future<void> incrementRetryCount(String msgId) async {
    final db = await database;
    final message = await getMessageById(msgId);
    if (message == null) return;
    
    await db.update(
      _tableName,
      {
        'retry_count': message.retryCount + 1,
        'last_retry_at': DateTime.now().millisecondsSinceEpoch,
      },
      where: 'msg_id = ?',
      whereArgs: [msgId],
    );
    print('🔄 Retry count incrementado: $msgId (${message.retryCount + 1})');
  }

  // ✅ Buscar mensagem por ID
  static Future<PendingMessage?> getMessageById(String msgId) async {
    final db = await database;
    final results = await db.query(
      _tableName,
      where: 'msg_id = ?',
      whereArgs: [msgId],
      limit: 1,
    );
    
    if (results.isEmpty) return null;
    return PendingMessage.fromMap(results.first);
  }

  // ✅ Deletar mensagem (quando confirmada pelo servidor)
  static Future<void> deleteMessage(String msgId) async {
    final db = await database;
    await db.delete(
      _tableName,
      where: 'msg_id = ?',
      whereArgs: [msgId],
    );
    print('🗑️ Mensagem deletada do storage local: $msgId');
  }

  // ✅ Atualizar conteúdo da mensagem (edit)
  static Future<void> updateMessageContent(String msgId, String newContent) async {
    final db = await database;
    await db.update(
      _tableName,
      {
        'content': newContent,
        'is_edited': 1,
      },
      where: 'msg_id = ?',
      whereArgs: [msgId],
    );
    print('✏️ Conteúdo atualizado: $msgId');
  }

  // ✅ Marcar mensagem como deletada (soft delete)
  static Future<void> markMessageAsDeleted(String msgId) async {
    final db = await database;
    await db.update(
      _tableName,
      {
        'is_deleted': 1,
        'content': '⊗ Eliminou esta mensagem', // ✅ Personalizar mensagem deletada
      },
      where: 'msg_id = ?',
      whereArgs: [msgId],
    );
    print('🗑️ Mensagem marcada como deletada: $msgId');
  }

  // ✅ Limpar mensagens sincronizadas (status != pending_local)
  static Future<void> cleanupSyncedMessages() async {
    final db = await database;
    final deleted = await db.delete(
      _tableName,
      where: 'status != ?',
      whereArgs: ['pending_local'],
    );
    print('🧹 Limpeza: $deleted mensagens sincronizadas removidas');
  }

  // ✅ Limpar mensagens antigas (mais de 30 dias e status != pending_local)
  static Future<void> cleanupOldMessages() async {
    final db = await database;
    final thirtyDaysAgo = DateTime.now().subtract(Duration(days: 30));
    
    await db.delete(
      _tableName,
      where: 'status != ? AND created_at < ?',
      whereArgs: ['pending_local', thirtyDaysAgo.millisecondsSinceEpoch],
    );
    print('🧹 Limpeza de mensagens antigas concluída');
  }

  // ✅ Contar mensagens pendentes
  static Future<int> countPendingMessages({String? status}) async {
    final db = await database;
    
    if (status != null) {
      return Sqflite.firstIntValue(
        await db.rawQuery(
          'SELECT COUNT(*) FROM $_tableName WHERE status = ?',
          [status],
        ),
      ) ?? 0;
    }
    
    return Sqflite.firstIntValue(
      await db.rawQuery('SELECT COUNT(*) FROM $_tableName WHERE status = ?', ['pending_local']),
    ) ?? 0;
  }

  // ✅ Verificar se mensagem excedeu max retries
  static Future<bool> hasExceededMaxRetries(String msgId) async {
    final message = await getMessageById(msgId);
    if (message == null) return false;
    return message.retryCount >= _maxRetries;
  }
}

