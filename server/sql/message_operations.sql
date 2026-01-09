-- message_operations.sql
-- Schema para operações avançadas de mensagens

-- 1. Message References (para respostas)
CREATE TABLE IF NOT EXISTS message_references (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    referenced_message_id INT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    created_at TIMESTAMPTZ DEFAULT now(),
    UNIQUE(message_id, referenced_message_id)
);

-- Index para performance
CREATE INDEX IF NOT EXISTS idx_message_references_msg ON message_references(message_id);
CREATE INDEX IF NOT EXISTS idx_message_references_ref ON message_references(referenced_message_id);

-- 2. Message Edit History (para auditoria e recuperação)
CREATE TABLE IF NOT EXISTS message_edit_history (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    original_content TEXT NOT NULL,
    edited_content TEXT NOT NULL,
    edited_by INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    edited_at TIMESTAMPTZ DEFAULT now(),
    ip_address INET,
    user_agent TEXT
);

-- Index para performance
CREATE INDEX IF NOT EXISTS idx_edit_history_msg ON message_edit_history(message_id);
CREATE INDEX IF NOT EXISTS idx_edit_history_user ON message_edit_history(edited_by);

-- 3. Message Delete Log (para admin recuperar)
CREATE TABLE IF NOT EXISTS message_delete_log (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    deleted_by INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    original_content TEXT NOT NULL,
    delete_reason VARCHAR(100) DEFAULT 'user_deleted',
    deleted_at TIMESTAMPTZ DEFAULT now(),
    ip_address INET,
    user_agent TEXT,
    is_admin_recoverable BOOLEAN DEFAULT true
);

-- Index para performance
CREATE INDEX IF NOT EXISTS idx_delete_log_msg ON message_delete_log(message_id);
CREATE INDEX IF NOT EXISTS idx_delete_log_user ON message_delete_log(deleted_by);
CREATE INDEX IF NOT EXISTS idx_delete_log_recoverable ON message_delete_log(is_admin_recoverable);

-- 4. Message Notifications (para notificações de edição)
CREATE TABLE IF NOT EXISTS message_notifications (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL REFERENCES messages(id) ON DELETE CASCADE,
    user_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
    notification_type VARCHAR(20) NOT NULL CHECK (notification_type IN ('edited', 'deleted', 'reply')),
    is_read BOOLEAN DEFAULT false,
    created_at TIMESTAMPTZ DEFAULT now(),
    expires_at TIMESTAMPTZ DEFAULT (now() + interval '7 days')
);

-- Index para performance e cleanup
CREATE INDEX IF NOT EXISTS idx_notifications_msg ON message_notifications(message_id);
CREATE INDEX IF NOT EXISTS idx_notifications_user ON message_notifications(user_id);
CREATE INDEX IF NOT EXISTS idx_notifications_unread ON message_notifications(user_id, is_read);
CREATE INDEX IF NOT EXISTS idx_notifications_expires ON message_notifications(expires_at);

-- 5. Atualizar tabela messages com campos adicionais
ALTER TABLE messages 
ADD COLUMN IF NOT EXISTS reply_to_id INT REFERENCES messages(id),
ADD COLUMN IF NOT EXISTS is_edited BOOLEAN DEFAULT false,
ADD COLUMN IF NOT EXISTS edit_count INT DEFAULT 0,
ADD COLUMN IF NOT EXISTS deleted_by INT REFERENCES users(id),
ADD COLUMN IF NOT EXISTS delete_reason VARCHAR(100),
ADD COLUMN IF NOT EXISTS deleted_at TIMESTAMPTZ,
ADD COLUMN IF NOT EXISTS edited_at TIMESTAMPTZ;

-- Index para novas colunas
CREATE INDEX IF NOT EXISTS idx_messages_reply_to ON messages(reply_to_id);
CREATE INDEX IF NOT EXISTS idx_messages_deleted_by ON messages(deleted_by);

-- 6. Função para soft delete
CREATE OR REPLACE FUNCTION soft_delete_message(
    p_message_id INT,
    p_user_id INT,
    p_reason VARCHAR(100) DEFAULT 'user_deleted'
) RETURNS BOOLEAN AS $$
DECLARE
    v_sender_id INT;
    v_receiver_id INT;
    v_content TEXT;
BEGIN
    -- Verificar se mensagem existe e obter dados
    SELECT sender_id, receiver_id, content 
    INTO v_sender_id, v_receiver_id, v_content
    FROM messages 
    WHERE id = p_message_id AND is_deleted = false;
    
    IF NOT FOUND THEN
        RETURN FALSE;
    END IF;
    
    -- Verificar permissões
    IF v_sender_id != p_user_id AND v_receiver_id != p_user_id THEN
        RETURN FALSE;
    END IF;
    
    -- Salvar no log de deleção
    INSERT INTO message_delete_log (
        message_id, deleted_by, original_content, delete_reason
    ) VALUES (
        p_message_id, p_user_id, v_content, p_reason
    );
    
    -- Soft delete na tabela messages
    UPDATE messages 
    SET is_deleted = true, 
        deleted_by = p_user_id,
        delete_reason = p_reason,
        deleted_at = now()
    WHERE id = p_message_id;
    
    -- Criar notificação para outros participantes
    IF v_sender_id = p_user_id THEN
        -- Se quem deletou foi o remetente, notificar destinatário
        INSERT INTO message_notifications (message_id, user_id, notification_type)
        VALUES (p_message_id, v_receiver_id, 'deleted');
    ELSE
        -- Se quem deletou foi o destinatário, notificar remetente
        INSERT INTO message_notifications (message_id, user_id, notification_type)
        VALUES (p_message_id, v_sender_id, 'deleted');
    END IF;
    
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

-- 7. Função para editar mensagem
CREATE OR REPLACE FUNCTION edit_message(
    p_message_id INT,
    p_user_id INT,
    p_new_content TEXT
) RETURNS BOOLEAN AS $$
DECLARE
    v_original_content TEXT;
    v_sender_id INT;
    v_receiver_id INT;
    v_sent_at TIMESTAMPTZ;
BEGIN
    -- Verificar se mensagem existe e pertence ao usuário
    SELECT sender_id, receiver_id, content, sent_at
    INTO v_sender_id, v_receiver_id, v_original_content, v_sent_at
    FROM messages 
    WHERE id = p_message_id AND sender_id = p_user_id AND is_deleted = false;
    
    IF NOT FOUND THEN
        RETURN FALSE;
    END IF;
    
    -- Verificar tempo limite (15 minutos)
    IF EXTRACT(EPOCH FROM (now() - v_sent_at)) > 900 THEN -- 15 minutos em segundos
        RETURN FALSE;
    END IF;
    
    -- Salvar no histórico de edições
    INSERT INTO message_edit_history (
        message_id, original_content, edited_content, edited_by
    ) VALUES (
        p_message_id, v_original_content, p_new_content, p_user_id
    );
    
    -- Atualizar mensagem
    UPDATE messages 
    SET content = p_new_content,
        is_edited = true,
        edit_count = edit_count + 1,
        edited_at = now()
    WHERE id = p_message_id;
    
    -- Criar notificação para destinatário
    INSERT INTO message_notifications (message_id, user_id, notification_type)
    VALUES (p_message_id, v_receiver_id, 'edited');
    
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

-- 8. Função para responder mensagem
CREATE OR REPLACE FUNCTION reply_to_message(
    p_original_message_id INT,
    p_reply_content TEXT,
    p_sender_id INT,
    p_receiver_id INT DEFAULT NULL,
    p_group_id INT DEFAULT NULL
) RETURNS INT AS $$
DECLARE
    v_new_message_id INT;
    v_original_sender_id INT;
    v_original_receiver_id INT;
BEGIN
    -- Verificar se mensagem original existe e não está deletada
    SELECT sender_id, receiver_id
    INTO v_original_sender_id, v_original_receiver_id
    FROM messages 
    WHERE id = p_original_message_id AND is_deleted = false;
    
    IF NOT FOUND THEN
        RETURN NULL;
    END IF;
    
    -- Se receiver_id não foi fornecido, determinar automaticamente
    DECLARE
        v_final_receiver_id INT := p_receiver_id;
    BEGIN
        IF p_receiver_id IS NULL THEN
            -- Se original foi enviada para mim, respondo para remetente
            IF p_sender_id = v_original_receiver_id THEN
                v_final_receiver_id := v_original_sender_id;
            ELSE
                v_final_receiver_id := v_original_receiver_id;
            END IF;
        END IF;
        
        -- Criar nova mensagem
        INSERT INTO messages (
            sender_id, receiver_id, group_id, content, reply_to_id, status
        ) VALUES (
            p_sender_id, v_final_receiver_id, p_group_id, p_reply_content, 
            p_original_message_id, 'sent'
        ) RETURNING id INTO v_new_message_id;
        
        -- Criar referência
        INSERT INTO message_references (message_id, referenced_message_id)
        VALUES (v_new_message_id, p_original_message_id);
        
        -- Criar notificação para mensagem respondida
        INSERT INTO message_notifications (message_id, user_id, notification_type)
        VALUES (v_new_message_id, v_final_receiver_id, 'reply');
        
        RETURN v_new_message_id;
    END;
END;
$$ LANGUAGE plpgsql;

-- 9. Função para admin recuperar mensagens deletadas
CREATE OR REPLACE FUNCTION admin_recover_message(
    p_message_id INT,
    p_admin_id INT
) RETURNS BOOLEAN AS $$
BEGIN
    -- Verificar se é admin (simplificado)
    -- Implementar verificação real de admin conforme seu sistema
    IF NOT EXISTS (
        SELECT 1 FROM user_roles ur
        JOIN roles r ON ur.role_id = r.id
        WHERE ur.user_id = p_admin_id AND r.name = 'Admin'
    ) THEN
        RETURN FALSE;
    END IF;
    
    -- Verificar se mensagem está deletada
    IF NOT EXISTS (
        SELECT 1 FROM messages 
        WHERE id = p_message_id AND is_deleted = true
    ) THEN
        RETURN FALSE;
    END IF;
    
    -- Recuperar mensagem
    UPDATE messages 
    SET is_deleted = false,
        deleted_by = NULL,
        delete_reason = NULL,
        deleted_at = NULL
    WHERE id = p_message_id;
    
    RETURN TRUE;
END;
$$ LANGUAGE plpgsql;

-- 10. Cleanup automático de notificações expiradas
CREATE OR REPLACE FUNCTION cleanup_expired_notifications() RETURNS VOID AS $$
BEGIN
    DELETE FROM message_notifications 
    WHERE expires_at < now();
END;
$$ LANGUAGE plpgsql;

-- 11. Trigger para cleanup automático
CREATE OR REPLACE FUNCTION trigger_cleanup_notifications()
RETURNS TRIGGER AS $$
BEGIN
    PERFORM cleanup_expired_notifications();
    RETURN NEW;
END;
$$ LANGUAGE plpgsql;

CREATE TRIGGER auto_cleanup_notifications
AFTER INSERT ON message_notifications
EXECUTE FUNCTION trigger_cleanup_notifications();