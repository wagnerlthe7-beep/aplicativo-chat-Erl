// chat_model.dart
import 'dart:typed_data';
import 'dart:convert';

class ChatContact {
  final String contactId;
  final String name;
  final String? phoneNumber;
  final Uint8List? photo; // ✅ AGORA CORRETO
  final DateTime lastMessageTime;
  final String lastMessage;
  final int unreadCount;

  ChatContact({
    required this.contactId,
    required this.name,
    this.phoneNumber,
    this.photo,
    required this.lastMessageTime,
    required this.lastMessage,
    required this.unreadCount,
  });

  Map<String, dynamic> toMap() {
    return {
      'contactId': contactId,
      'name': name,
      'phoneNumber': phoneNumber,
      'photo': photo != null
          ? base64Encode(photo!)
          : null, // ✅ CONVERTER PARA BASE64
      'lastMessageTime': lastMessageTime.millisecondsSinceEpoch,
      'lastMessage': lastMessage,
      'unreadCount': unreadCount,
    };
  }

  factory ChatContact.fromMap(Map<String, dynamic> map) {
    return ChatContact(
      contactId: map['contactId'],
      name: map['name'],
      phoneNumber: map['phoneNumber'],
      photo: map['photo'] != null
          ? base64Decode(map['photo'])
          : null, // ✅ CONVERTER DE BASE64
      lastMessageTime: DateTime.fromMillisecondsSinceEpoch(
        map['lastMessageTime'],
      ),
      lastMessage: map['lastMessage'],
      unreadCount: map['unreadCount'],
    );
  }

  // ✅ COPY-WITH METHOD
  ChatContact copyWith({
    String? name,
    String? phoneNumber,
    Uint8List? photo,
    DateTime? lastMessageTime,
    String? lastMessage,
    int? unreadCount,
  }) {
    return ChatContact(
      contactId: contactId,
      name: name ?? this.name,
      phoneNumber: phoneNumber ?? this.phoneNumber,
      photo: photo ?? this.photo,
      lastMessageTime: lastMessageTime ?? this.lastMessageTime,
      lastMessage: lastMessage ?? this.lastMessage,
      unreadCount: unreadCount ?? this.unreadCount,
    );
  }
}
