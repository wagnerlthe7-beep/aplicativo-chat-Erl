// models/pending_message.dart
// Modelo para mensagens pendentes (offline-first)

class PendingMessage {
  final String msgId; // UUID gerado localmente
  final String to; // UserId do destinatário
  final String from; // UserId do remetente
  final String content; // Conteúdo da mensagem
  final String status; // pending_local, sent, delivered, read
  final DateTime createdAt; // Timestamp de criação
  final String? dbMessageId; // ID do servidor (quando confirmado)
  final int retryCount; // Número de tentativas de envio
  final DateTime? lastRetryAt; // Última tentativa de envio
  // ✅ Campos de reply
  final String? replyToId; // ID da mensagem respondida
  final String? replyToText; // Texto da mensagem respondida
  final String? replyToSenderName; // Nome de quem enviou a mensagem respondida
  final String? replyToSenderId; // ID de quem enviou a mensagem respondida
  // ✅ Campos de edição e deleção
  final bool isEdited; // Se a mensagem foi editada
  final bool isDeleted; // Se a mensagem foi deletada

  PendingMessage({
    required this.msgId,
    required this.to,
    required this.from,
    required this.content,
    required this.status,
    required this.createdAt,
    this.dbMessageId,
    this.retryCount = 0,
    this.lastRetryAt,
    this.replyToId,
    this.replyToText,
    this.replyToSenderName,
    this.replyToSenderId,
    this.isEdited = false,
    this.isDeleted = false,
  });

  Map<String, dynamic> toMap() {
    return {
      'msg_id': msgId,
      'to_user': to, // ✅ Corrigido: usar to_user para corresponder à tabela
      'from_user':
          from, // ✅ Corrigido: usar from_user para corresponder à tabela
      'content': content,
      'status': status,
      'created_at': createdAt.millisecondsSinceEpoch,
      'db_message_id': dbMessageId,
      'retry_count': retryCount,
      'last_retry_at': lastRetryAt?.millisecondsSinceEpoch,
      // ✅ Campos de reply
      'reply_to_id': replyToId,
      'reply_to_text': replyToText,
      'reply_to_sender_name': replyToSenderName,
      'reply_to_sender_id': replyToSenderId,
      // ✅ Campos de edição e deleção
      'is_edited': isEdited ? 1 : 0,
      'is_deleted': isDeleted ? 1 : 0,
    };
  }

  factory PendingMessage.fromMap(Map<String, dynamic> map) {
    return PendingMessage(
      msgId: map['msg_id'],
      to: map['to_user'] ?? map['to'], // ✅ Suporta ambos para compatibilidade
      from:
          map['from_user'] ??
          map['from'], // ✅ Suporta ambos para compatibilidade
      content: map['content'],
      status: map['status'],
      createdAt: DateTime.fromMillisecondsSinceEpoch(map['created_at']),
      dbMessageId: map['db_message_id'],
      retryCount: map['retry_count'] ?? 0,
      lastRetryAt: map['last_retry_at'] != null
          ? DateTime.fromMillisecondsSinceEpoch(map['last_retry_at'])
          : null,
      // ✅ Campos de reply
      replyToId: map['reply_to_id'],
      replyToText: map['reply_to_text'],
      replyToSenderName: map['reply_to_sender_name'],
      replyToSenderId: map['reply_to_sender_id'],
      // ✅ Campos de edição e deleção
      isEdited: (map['is_edited'] ?? 0) == 1,
      isDeleted: (map['is_deleted'] ?? 0) == 1,
    );
  }

  PendingMessage copyWith({
    String? msgId,
    String? to,
    String? from,
    String? content,
    String? status,
    DateTime? createdAt,
    String? dbMessageId,
    int? retryCount,
    DateTime? lastRetryAt,
    String? replyToId,
    String? replyToText,
    String? replyToSenderName,
    String? replyToSenderId,
    bool? isEdited,
    bool? isDeleted,
  }) {
    return PendingMessage(
      msgId: msgId ?? this.msgId,
      to: to ?? this.to,
      from: from ?? this.from,
      content: content ?? this.content,
      status: status ?? this.status,
      createdAt: createdAt ?? this.createdAt,
      dbMessageId: dbMessageId ?? this.dbMessageId,
      retryCount: retryCount ?? this.retryCount,
      lastRetryAt: lastRetryAt ?? this.lastRetryAt,
      replyToId: replyToId ?? this.replyToId,
      replyToText: replyToText ?? this.replyToText,
      replyToSenderName: replyToSenderName ?? this.replyToSenderName,
      replyToSenderId: replyToSenderId ?? this.replyToSenderId,
      isEdited: isEdited ?? this.isEdited,
      isDeleted: isDeleted ?? this.isDeleted,
    );
  }
}
