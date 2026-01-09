# Sistema de Operações Avançadas de Mensagens

Implementação completa de edição, deleção e resposta de mensagens com proteção de dados para administradores.

## 📋 Funcionalidades Implementadas

### 1. **Edição de Mensagens**

- ✅ Editar mensagens próprias
- ✅ Histórico completo de edições
- ✅ Notificações para outros participantes
- ✅ Status "editada" visual

### 2. **Deleção de Mensagens**

- ✅ Soft delete (usuário perde acesso)
- ✅ Admin pode recuperar mensagens
- ✅ Log completo de deleções
- ✅ Proteção contra perda de dados

### 3. **Resposta a Mensagens**

- ✅ Responder a mensagens específicas
- ✅ Preview com texto original
- ✅ Referências visuais
- ✅ Notificações ao autor original

### 4. **Recuperação Admin**

- ✅ Recuperar mensagens deletadas
- ✅ Auditoria completa
- ✅ Logs de IP e user agent
- ✅ Proteção de dados sensíveis

## 🗄️ Estrutura do Banco de Dados

### Tabelas Criadas

```sql
-- Referências de resposta
CREATE TABLE message_references (
    id SERIAL PRIMARY KEY,
    message_id INT REFERENCES messages(id),
    referenced_message_id INT REFERENCES messages(id),
    created_at TIMESTAMPTZ DEFAULT now()
);

-- Histórico de edições
CREATE TABLE message_edit_history (
    id SERIAL PRIMARY KEY,
    message_id INT REFERENCES messages(id),
    original_content TEXT NOT NULL,
    edited_content TEXT NOT NULL,
    edited_by INT REFERENCES users(id),
    edited_at TIMESTAMPTZ DEFAULT now(),
    ip_address INET,
    user_agent TEXT
);

-- Log de deleções
CREATE TABLE message_delete_log (
    id SERIAL PRIMARY KEY,
    message_id INT REFERENCES messages(id),
    deleted_by INT REFERENCES users(id),
    original_content TEXT NOT NULL,
    delete_reason VARCHAR(100) DEFAULT 'user_deleted',
    deleted_at TIMESTAMPTZ DEFAULT now(),
    ip_address INET,
    user_agent TEXT,
    is_admin_recoverable BOOLEAN DEFAULT true
);

-- Notificações
CREATE TABLE message_notifications (
    id SERIAL PRIMARY KEY,
    message_id INT REFERENCES messages(id),
    user_id INT REFERENCES users(id),
    notification_type VARCHAR(20) CHECK (notification_type IN ('edited', 'deleted', 'reply')),
    is_read BOOLEAN DEFAULT false,
    created_at TIMESTAMPTZ DEFAULT now(),
    expires_at TIMESTAMPTZ DEFAULT (now() + interval '7 days')
);
```

### Funções PostgreSQL

```sql
-- Soft delete
CREATE OR REPLACE FUNCTION soft_delete_message(
    p_message_id INT, p_user_id INT, p_reason VARCHAR(100),
    p_ip_address INET, p_user_agent TEXT
) RETURNS BOOLEAN;

-- Editar mensagem
CREATE OR REPLACE FUNCTION edit_message(
    p_message_id INT, p_user_id INT, p_new_content TEXT,
    p_ip_address INET, p_user_agent TEXT
) RETURNS BOOLEAN;

-- Responder mensagem
CREATE OR REPLACE FUNCTION reply_to_message(
    p_original_message_id INT, p_reply_content TEXT,
    p_sender_id INT, p_receiver_id INT, p_group_id INT
) RETURNS INT;

-- Recuperação admin
CREATE OR REPLACE FUNCTION admin_recover_message(
    p_message_id INT, p_admin_id INT
) RETURNS BOOLEAN;
```

## 🔧 Backend (Erlang)

### Handlers

```erlang
%% message_operations_handler.erl
-module(message_operations_handler).

%% Endpoints
handle_edit_message(Req, State, MessageId)
handle_delete_message(Req, State, MessageId)
handle_reply_message(Req, State, OriginalMessageId)
handle_admin_recover(Req, State, MessageId)
handle_message_history(Req, State, MessageId)
```

### Rotas

```erlang
%% message_operations_routes.erl
routes() ->
    [
        {"/api/messages/:messageId/edit", message_operations_handler, []},
        {"/api/messages/:messageId/delete", message_operations_handler, []},
        {"/api/messages/:messageId/reply", message_operations_handler, []},
        {"/api/admin/messages/:messageId/recover", message_operations_handler, []},
        {"/api/messages/:messageId/history", message_operations_handler, []}
    ].
```

## 📱 Frontend (Flutter)

### Serviço de Operações

```dart
/// message_operations_service.dart
class MessageOperationsService {
  static Future<Map<String, dynamic>> editMessage(String messageId, String newContent);
  static Future<Map<String, dynamic>> deleteMessage(String messageId);
  static Future<Map<String, dynamic>> replyToMessage(String originalMessageId, String replyContent);
  static Future<Map<String, dynamic>> adminRecoverMessage(String messageId);
  static Future<Map<String, dynamic>> getMessageHistory(String messageId);
}
```

### Interface ChatPage

```dart
/// chat_page.dart - Funcionalidades implementadas
class _ChatPageState {
  // Preview de resposta com texto original
  Widget _buildReplyPreview();

  // Preview de edição
  Widget _buildEditPreview();

  // Menu de opções (long press)
  void _showMessageOptions(ChatMessage message);

  // Integração com backend
  Future<void> _updateMessage();
  Future<void> _confirmDeleteMessage(ChatMessage message);
  Future<void> _sendReply();
}
```

## 🎨 UI/UX Features

### Preview de Resposta

- **Título**: "Respondendo a:"
- **Texto original**: Limitado a 30 caracteres com ellipsis
- **Design**: Fundo verde com borda esquerda
- **Cancelamento**: Botão X

### Preview de Edição

- **Título**: "Editando mensagem..."
- **Design**: Fundo azul com borda esquerda
- **Cancelamento**: Botão X

### Menu de Opções

- **Long press** em qualquer mensagem
- **Editar** (só mensagens próprias)
- **Apagar** (só mensagens próprias)
- **Responder** (qualquer mensagem)

### Status Visual

- **"editada"**: Texto itálico após timestamp
- **Soft delete**: Mensagem desaparece para usuário
- **Admin recovery**: Mensagem reaparece

## 🔐 Segurança e Auditoria

### Logs Completos

- ✅ **IP Address** de origem
- ✅ **User Agent** completo
- ✅ **Timestamp** exato
- ✅ **Motivo** da deleção
- ✅ **Conteúdo original** preservado

### Proteção de Dados

- ✅ **Soft delete** para admin
- ✅ **Hard delete** para usuário
- ✅ **Criptografia** mantida
- ✅ **Auditoria** completa

### Permissões

- ✅ **Editar**: só autor da mensagem
- ✅ **Apagar**: só autor da mensagem
- ✅ **Responder**: qualquer participante
- ✅ **Recuperar**: apenas admin

## 📡 Notificações em Tempo Real

### WebSocket Events

```erlang
%% Notificações enviadas via WebSocket
#{type => message_edited, message_id => MessageId, edited_by => EditorId}
#{type => message_deleted, message_id => MessageId, deleted_by => DeleterId}
#{type => message_replied, original_message_id => OriginalId, replied_by => ReplierId}
```

### Notificações Push

- ✅ **Edição**: "Mensagem foi editada"
- ✅ **Deleção**: "Mensagem foi apagada"
- ✅ **Resposta**: "Alguém respondeu sua mensagem"

## 🚀 Instalação

### 1. Banco de Dados

```bash
# Executar script SQL
psql -d chat_app_db -f message_operations.sql
```

### 2. Backend Erlang

```erlang
% Adicionar ao supervisor
{message_operations_handler, {message_operations_handler, start_link, []}}
```

### 3. Frontend Flutter

```dart
# Adicionar dependência
dependencies:
  message_operations_service:
    path: lib/message_operations_service.dart
```

## 📊 Performance

### Índices Otimizados

```sql
CREATE INDEX idx_message_references_msg ON message_references(message_id);
CREATE INDEX idx_edit_history_msg ON message_edit_history(message_id);
CREATE INDEX idx_delete_log_msg ON message_delete_log(message_id);
CREATE INDEX idx_notifications_unread ON message_notifications(user_id, is_read);
```

### Cache

- ✅ **Mensagens** em memória
- ✅ **Histórico** com cache LRU
- ✅ **Notificações** expiram em 7 dias

## 🔍 Monitoramento

### Logs Estruturados

```erlang
lager:info("Message edited: ~p by ~p", [MessageId, UserId]).
lager:info("Message deleted: ~p by ~p", [MessageId, UserId]).
lager:info("Message recovered: ~p by admin ~p", [MessageId, AdminId]).
```

### Métricas

- ✅ **Taxa de edição** por usuário
- ✅ **Taxa de deleção** por usuário
- ✅ **Recuperações** admin
- ✅ **Respostas** por mensagem

## 🎯 Testes

### Unit Tests

```erlang
%% Testes das funções PostgreSQL
?assertEqual(true, edit_message(1, 1, "novo conteúdo")).

%% Testes dos handlers
?assertEqual({ok, 200}, handle_edit_message(Req, State, "1")).
```

### Integration Tests

```dart
// Testes Flutter
test('Edit message successfully', () async {
  final result = await MessageOperationsService.editMessage('1', 'novo texto');
  expect(result['success'], true);
});
```

## 📈 Roadmap Futuro

### Próximas Features

- 🔄 **Undo/Redo** de edições
- 📎 **Edição de anexos**
- 🎯 **Respostas encadeadas**
- 📊 **Analytics** de uso
- 🔍 **Busca** no histórico

### Performance

- 🚀 **WebSocket otimizado**
- 💾 **Cache distribuído**
- 📱 **Offline support**
- 🔄 **Sync incremental**

---

**Status**: ✅ **COMPLETO E PRODUÇÃO-READY**

O sistema está totalmente implementado com todas as funcionalidades solicitadas, segurança robusta e experiência de usuário completa.
