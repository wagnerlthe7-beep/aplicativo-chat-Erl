// chat_model.dart
import 'dart:typed_data';
import 'dart:convert';

class ChatContact {
  final String contactId;
  final String name;
  final String? phoneNumber;
  final Uint8List? photo;
  final DateTime lastMessageTime;
  final String lastMessage;
  final int unreadCount;
  final bool lastMessageEdited;
  final bool lastMessageIsReply;

  ChatContact({
    required this.contactId,
    required this.name,
    this.phoneNumber,
    this.photo,
    required this.lastMessageTime,
    required this.lastMessage,
    required this.unreadCount,
    this.lastMessageEdited = false,
    this.lastMessageIsReply = false,
  });

  Map<String, dynamic> toMap() {
    return {
      'contactId': contactId,
      'name': name,
      'phoneNumber': phoneNumber,
      'photo': photo != null ? base64Encode(photo!) : null,
      'lastMessageTime': lastMessageTime.millisecondsSinceEpoch,
      'lastMessage': lastMessage,
      'unreadCount': unreadCount,
      'lastMessageEdited': lastMessageEdited,
      'lastMessageIsReply': lastMessageIsReply,
    };
  }

  factory ChatContact.fromMap(Map<String, dynamic> map) {
    return ChatContact(
      contactId: map['contactId'],
      name: map['name'],
      phoneNumber: map['phoneNumber'],
      photo: map['photo'] != null ? base64Decode(map['photo']) : null,
      lastMessageTime: DateTime.fromMillisecondsSinceEpoch(
        map['lastMessageTime'],
      ),
      lastMessage: map['lastMessage'],
      unreadCount: map['unreadCount'],
      lastMessageEdited: map['lastMessageEdited'] ?? false,
      lastMessageIsReply: map['lastMessageIsReply'] ?? false,
    );
  }

  // ✅ COPY-WITH METHOD atualizado
  ChatContact copyWith({
    String? name,
    String? phoneNumber,
    Uint8List? photo,
    DateTime? lastMessageTime,
    String? lastMessage,
    int? unreadCount,
    bool? lastMessageEdited,
    bool? lastMessageIsReply,
  }) {
    return ChatContact(
      contactId: contactId,
      name: name ?? this.name,
      phoneNumber: phoneNumber ?? this.phoneNumber,
      photo: photo ?? this.photo,
      lastMessageTime: lastMessageTime ?? this.lastMessageTime,
      lastMessage: lastMessage ?? this.lastMessage,
      unreadCount: unreadCount ?? this.unreadCount,
      lastMessageEdited: lastMessageEdited ?? this.lastMessageEdited,
      lastMessageIsReply: lastMessageIsReply ?? this.lastMessageIsReply,
    );
  }
}

// Modelo para mensagens editadas
class EditedMessage {
  final String id;
  final String originalContent;
  final String editedContent;
  final String editorId;
  final String editorName;
  final DateTime editedAt;

  EditedMessage({
    required this.id,
    required this.originalContent,
    required this.editedContent,
    required this.editorId,
    required this.editorName,
    required this.editedAt,
  });

  factory EditedMessage.fromMap(Map<String, dynamic> map) {
    return EditedMessage(
      id: map['id'].toString(),
      originalContent: map['original_content'],
      editedContent: map['edited_content'],
      editorId: map['edited_by'].toString(),
      editorName: map['editor_name'],
      editedAt: DateTime.parse(map['edited_at']),
    );
  }
}
