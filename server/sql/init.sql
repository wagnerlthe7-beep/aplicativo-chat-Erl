-- init.sql
-- Schema inicial para chat_app_db (PostgreSQL)
--
-- ======================
-- 1. Users
-- ======================
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100),
    phone VARCHAR(20) UNIQUE NOT NULL,
    is_active BOOLEAN DEFAULT true,
    created_at TIMESTAMPTZ DEFAULT now(),
    last_login TIMESTAMPTZ
);

ALTER TABLE users
  ADD COLUMN IF NOT EXISTS firebase_uid TEXT UNIQUE;
  
ALTER TABLE users
  ADD COLUMN is_verified BOOLEAN DEFAULT false,
  ADD COLUMN profile_picture VARCHAR(255),
  ADD COLUMN last_seen TIMESTAMPTZ;


-- ======================
-- INICIO SECURE SESSION
-- capacidade de revogar sessões antigas (ex.: quando o telefone é roubado ou o usuário ativa nova sessão). 
-- É exatamente como o WhatsApp faz — session tokens persistentes até que o servidor (ou o usuário) os revogue. 
-- Assim, ele deve matar a sessao que esta no device antigo, para evitar acesso indevido. 
-- Isso eh feito principalmente nos campos refresh_token_has, device_info, created_at. e indicamos o campo revoked, para deixar de estar ativo
-- ======================
CREATE TABLE IF NOT EXISTS sessions (
  id BIGSERIAL PRIMARY KEY,
  user_id INT NOT NULL REFERENCES users(id) ON DELETE CASCADE,
  device_uuid VARCHAR(200) NOT NULL,
  device_info TEXT,
  refresh_token_hash BYTEA NOT NULL UNIQUE,
  created_at TIMESTAMPTZ NOT NULL DEFAULT now(),
  last_used_at TIMESTAMPTZ,
  expires_at TIMESTAMPTZ NULL,   -- NULL = sem expiração automática
  revoked BOOLEAN NOT NULL DEFAULT FALSE
);

CREATE INDEX IF NOT EXISTS idx_sessions_user_id ON sessions(user_id);
CREATE INDEX IF NOT EXISTS idx_sessions_device_uuid ON sessions(device_uuid);
CREATE INDEX IF NOT EXISTS idx_sessions_hash ON sessions USING btree (refresh_token_hash);
--Observações:
--Guardamos apenas o hash do refresh_token (coluna refresh_token_hash tipo BYTEA).
--expires_at fica NULL (sessão indefinida). A sessão é inválida só se revoked = true (ou se tu implementares expiracão manual depois).
-- FIM SECURE SESSION



-- ======================
-- 2. Roles (ex.: Admin, Manager, User, Auditor)
-- ======================
CREATE TABLE roles (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL,
    description VARCHAR(150)
);

-- ======================
-- 3. Permissions (actions allowed)
-- ======================
CREATE TABLE permissions (
    id SERIAL PRIMARY KEY,
    description VARCHAR(100) NOT NULL
);

-- ======================
-- 4. Role ↔ Permissions (N:N)
-- ======================
CREATE TABLE role_permissions (
    role_id INT,
    permission_id INT,
    PRIMARY KEY (role_id, permission_id),
    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE,
    FOREIGN KEY (permission_id) REFERENCES permissions(id) ON DELETE CASCADE
);

-- ======================
-- 5. User ↔ Roles (N:N)
-- ======================
CREATE TABLE user_roles (
    user_id INT,
    role_id INT,
    PRIMARY KEY (user_id, role_id),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE
);

-- ======================
-- 6. Chat Groups
-- ======================
CREATE TABLE chat_groups (
    id SERIAL PRIMARY KEY,
    name VARCHAR(100) NOT NULL,
    description TEXT,
    created_by INT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    is_private BOOLEAN DEFAULT false,
    group_type VARCHAR(20) DEFAULT 'private' CHECK (group_type IN ('private','public','broadcast')),
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE CASCADE
);

-- ======================
-- 7. Group Members
-- ======================
CREATE TABLE group_members (
    group_id INT,
    user_id INT,
    role VARCHAR(20) DEFAULT 'member' CHECK (role IN ('admin','member')),
    joined_at TIMESTAMPTZ DEFAULT now(),
    PRIMARY KEY (group_id, user_id),
    FOREIGN KEY (group_id) REFERENCES chat_groups(id) ON DELETE CASCADE,
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- ======================
-- 8. Messages
-- ======================
CREATE TABLE messages (
    id SERIAL PRIMARY KEY,
    sender_id INT NOT NULL,
    receiver_id INT,        -- private
    group_id INT,           -- group
    content TEXT,
    message_type VARCHAR(20) DEFAULT 'text' CHECK (message_type IN ('text','image','video','audio','document','system')),
    sent_at TIMESTAMPTZ DEFAULT now(),
    status VARCHAR(20) DEFAULT 'sent' CHECK (status IN ('sent','delivered','read')),
    is_deleted BOOLEAN DEFAULT false,
    edited_at TIMESTAMPTZ,
    FOREIGN KEY (sender_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (receiver_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (group_id) REFERENCES chat_groups(id) ON DELETE CASCADE,
    CONSTRAINT chk_target CHECK (
        (receiver_id IS NOT NULL AND group_id IS NULL) OR
        (receiver_id IS NULL AND group_id IS NOT NULL)
    )
);

-- Indexes
CREATE INDEX idx_messages_group ON messages(group_id, sent_at);
CREATE INDEX idx_messages_user ON messages(sender_id, sent_at);

-- ======================
-- 9. Attachments
-- ======================
CREATE TABLE attachments (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL,
    file_type VARCHAR(20) NOT NULL,           -- image, video, audio, document
    mime_type VARCHAR(50),
    file_path VARCHAR(255) NOT NULL,          -- ex: /uploads/2025/09/file.png
    thumbnail_path VARCHAR(255),
    size INT,
    FOREIGN KEY (message_id) REFERENCES messages(id) ON DELETE CASCADE
);

CREATE INDEX idx_attachments_msg ON attachments(message_id);



CREATE TABLE sms_verifications (
    id SERIAL PRIMARY KEY,
    phone VARCHAR(20) NOT NULL,
    code VARCHAR(6) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL,
    is_used BOOLEAN DEFAULT false
);

CREATE INDEX idx_sms_phone ON sms_verifications(phone);


CREATE TABLE user_devices (
    id SERIAL PRIMARY KEY,
    user_id INT NOT NULL,
    device_uuid VARCHAR(100) NOT NULL,
    device_type VARCHAR(20) CHECK (device_type IN ('android','ios','web')),
    push_token VARCHAR(255),
    last_active TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);


CREATE TABLE user_keys (
    id SERIAL PRIMARY KEY,
    user_id INT NOT NULL UNIQUE,
    public_key TEXT NOT NULL,        -- chave pública do utilizador
    last_updated TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);


ALTER TABLE messages
  ALTER COLUMN content TYPE TEXT, -- ciphertext base64
  ADD COLUMN encryption_scheme VARCHAR(50) DEFAULT 'AES256-GCM';


CREATE TABLE session_keys (
    id SERIAL PRIMARY KEY,
    chat_id INT NOT NULL, -- pode ser user_id (1:1) ou group_id
    user_id INT NOT NULL,
    encrypted_key TEXT NOT NULL,  -- chave de sessão cifrada com a public_key do user
    created_at TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);


ALTER TABLE attachments
  ADD COLUMN encryption_scheme VARCHAR(50) DEFAULT 'AES256-GCM';

-- ======================
-- 17. Example roles, permissions, admin user + admin key (initial data)
-- ======================

-- Roles
INSERT INTO roles (name, description) VALUES
('Admin', 'Administrator with audit access'),
('Manager', 'Manager role'),
('User', 'Regular user');

-- Permissions (examples)
INSERT INTO permissions (description) VALUES
('CREATE_GROUP'), ('DELETE_GROUP'), ('ADD_USER'), ('REMOVE_USER'),
('SEND_MESSAGE'), ('DELETE_MESSAGE'), ('VIEW_LOGS'), ('REQUEST_ADMIN_KEY');

-- Map some role_permissions (Admin gets most)
INSERT INTO role_permissions (role_id, permission_id)
SELECT r.id, p.id FROM roles r CROSS JOIN permissions p WHERE r.name = 'Admin';

-- Create an example admin user
INSERT INTO users (first_name, last_name, username, email, phone, is_verified, is_active)
VALUES ('System', 'Admin', 'admin_system', 'admin@chatapp.local', '849184975', TRUE, TRUE)
RETURNING id INTO TEMP TABLE temp_admin_id;

-- Note: above RETURNING INTO is psql-specific; if your psql client doesn't support temp table here
-- instead you can insert and SELECT the id afterwards:
--  SELECT id FROM users WHERE username='admin_system';

-- Get the admin id (portable way)
-- (We'll insert admin_keys using a simple SELECT matching username)
-- Insert admin public key placeholder
INSERT INTO admin_keys (admin_user_id, public_key, description)
SELECT id, 'ADMIN_PUBLIC_KEY_PLACEHOLDER_BASE64', 'Initial admin public key (replace with real key)' FROM users WHERE username = 'admin_system';

-- Create a chat and a corresponding chat_group as example
INSERT INTO chats (kind) VALUES ('group') RETURNING id INTO TEMP TABLE temp_chat_id;

-- Create a chat_group linked to chat
INSERT INTO chat_groups (chat_id, name, description, created_by, is_private, group_type)
SELECT (SELECT id FROM temp_chat_id), 'General Chat', 'All users general group', u.id, false, 'public'
FROM users u WHERE u.username = 'admin_system';

-- Clean up temp tables if exist (psql)
DROP TABLE IF EXISTS temp_chat_id;
DROP TABLE IF EXISTS temp_admin_id;

-- ======================
-- End of init.sql
-- ======================
