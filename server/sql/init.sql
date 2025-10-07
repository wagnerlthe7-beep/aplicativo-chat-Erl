-- init.sql
-- Schema inicial para chat_app_db (PostgreSQL)
-- Inclui: users, roles, permissions, chat_groups, messages, attachments,
-- sms_verifications, user_devices, user_keys, session_keys, admin_keys,
-- admin_session_keys, key_audit_log, chats (normalize), plus inserts iniciais.

-- NOTE: Execute este script em um database já criado (ex: chat_app_db).
--       Se preferires, cria o DB previamente:
--       CREATE DATABASE chat_app_db WITH ENCODING='UTF8' LC_COLLATE='en_US.utf8' LC_CTYPE='en_US.utf8' TEMPLATE=template0;

-- ============================
-- Drop existing (safe re-run)
-- ============================
DROP TABLE IF EXISTS key_audit_log CASCADE;
DROP TABLE IF EXISTS admin_session_keys CASCADE;
DROP TABLE IF EXISTS admin_keys CASCADE;
DROP TABLE IF EXISTS session_keys CASCADE;
DROP TABLE IF EXISTS user_keys CASCADE;
DROP TABLE IF EXISTS user_devices CASCADE;
DROP TABLE IF EXISTS sms_verifications CASCADE;
DROP TABLE IF EXISTS attachments CASCADE;
DROP TABLE IF EXISTS messages CASCADE;
DROP TABLE IF EXISTS group_members CASCADE;
DROP TABLE IF EXISTS chat_groups CASCADE;
DROP TABLE IF EXISTS user_roles CASCADE;
DROP TABLE IF EXISTS role_permissions CASCADE;
DROP TABLE IF EXISTS permissions CASCADE;
DROP TABLE IF EXISTS roles CASCADE;
DROP TABLE IF EXISTS chats CASCADE;

DROP TABLE IF EXISTS users CASCADE;

-- ======================
-- 0. Optional: chats table (abstract conversation entity)
-- ======================
CREATE TABLE chats (
    id SERIAL PRIMARY KEY,
    kind VARCHAR(20) NOT NULL CHECK (kind IN ('direct','group')) ,
    created_at TIMESTAMPTZ DEFAULT now(),
    -- if kind = 'group', link to chat_groups; if 'direct' could store meta
    UNIQUE (id)
);

-- ======================
-- 1. Users
-- ======================
CREATE TABLE users (
    id SERIAL PRIMARY KEY,
    first_name VARCHAR(100) NOT NULL,
    last_name VARCHAR(50),
    username VARCHAR(50) UNIQUE,
    email VARCHAR(100) UNIQUE,
    phone VARCHAR(20) UNIQUE NOT NULL,
    is_verified BOOLEAN DEFAULT false,
    is_active BOOLEAN DEFAULT true,
    profile_picture VARCHAR(255),
    created_at TIMESTAMPTZ DEFAULT now(),
    last_login TIMESTAMPTZ
);

-- ======================
-- 2. Roles
-- ======================
CREATE TABLE roles (
    id SERIAL PRIMARY KEY,
    name VARCHAR(50) NOT NULL UNIQUE,
    description VARCHAR(150)
);

-- ======================
-- 3. Permissions
-- ======================
CREATE TABLE permissions (
    id SERIAL PRIMARY KEY,
    description VARCHAR(100) NOT NULL UNIQUE
);

-- ======================
-- 4. Role ↔ Permissions (N:N)
-- ======================
CREATE TABLE role_permissions (
    role_id INT NOT NULL,
    permission_id INT NOT NULL,
    PRIMARY KEY (role_id, permission_id),
    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE,
    FOREIGN KEY (permission_id) REFERENCES permissions(id) ON DELETE CASCADE
);

-- ======================
-- 5. User ↔ Roles (N:N)
-- ======================
CREATE TABLE user_roles (
    user_id INT NOT NULL,
    role_id INT NOT NULL,
    PRIMARY KEY (user_id, role_id),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (role_id) REFERENCES roles(id) ON DELETE CASCADE
);

-- ======================
-- 6. Chat Groups (group metadata)
-- ======================
CREATE TABLE chat_groups (
    id SERIAL PRIMARY KEY,
    chat_id INT UNIQUE, -- optional link to chats.id
    name VARCHAR(100) NOT NULL,
    description TEXT,
    created_by INT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    is_private BOOLEAN DEFAULT false,
    group_type VARCHAR(20) DEFAULT 'private' CHECK (group_type IN ('private','public','broadcast')),
    FOREIGN KEY (created_by) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (chat_id) REFERENCES chats(id) ON DELETE SET NULL
);

-- ======================
-- 7. Group Members
-- ======================
CREATE TABLE group_members (
    group_id INT NOT NULL,
    user_id INT NOT NULL,
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
    chat_id INT NOT NULL,                 -- reference to chats.id
    sender_id INT NOT NULL,
    receiver_id INT,        -- for direct messages (nullable)
    group_id INT,           -- group reference (nullable) - optional duplicate/meta
    content TEXT,           -- ciphertext (base64) or plain depending on encryption
    message_type VARCHAR(20) DEFAULT 'text' CHECK (message_type IN ('text','image','video','audio','document','system')),
    encryption_scheme VARCHAR(100) DEFAULT 'X25519+HKDF+ChaCha20-Poly1305',
    sent_at TIMESTAMPTZ DEFAULT now(),
    status VARCHAR(20) DEFAULT 'sent' CHECK (status IN ('sent','delivered','read')),
    is_deleted BOOLEAN DEFAULT false,
    edited_at TIMESTAMPTZ,
    FOREIGN KEY (sender_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (receiver_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (group_id) REFERENCES chat_groups(id) ON DELETE CASCADE,
    FOREIGN KEY (chat_id) REFERENCES chats(id) ON DELETE CASCADE,
    CONSTRAINT chk_target CHECK (
        (receiver_id IS NOT NULL AND group_id IS NULL) OR
        (receiver_id IS NULL AND group_id IS NOT NULL)
    )
);

CREATE INDEX idx_messages_chat_sent_at ON messages(chat_id, sent_at);
CREATE INDEX idx_messages_sender_sent_at ON messages(sender_id, sent_at);

-- ======================
-- 9. Attachments
-- ======================
CREATE TABLE attachments (
    id SERIAL PRIMARY KEY,
    message_id INT NOT NULL,
    file_type VARCHAR(20) NOT NULL,           -- image, video, audio, document
    mime_type VARCHAR(50),
    file_path VARCHAR(255) NOT NULL,          -- path to encrypted blob
    thumbnail_path VARCHAR(255),
    size INT,
    encryption_scheme VARCHAR(50) DEFAULT 'AES256-GCM',
    is_encrypted BOOLEAN DEFAULT TRUE,
    FOREIGN KEY (message_id) REFERENCES messages(id) ON DELETE CASCADE
);

CREATE INDEX idx_attachments_message ON attachments(message_id);

-- ======================
-- 10. SMS verifications (one-time codes)
-- ======================
CREATE TABLE sms_verifications (
    id SERIAL PRIMARY KEY,
    phone VARCHAR(20) NOT NULL,
    code VARCHAR(6) NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    expires_at TIMESTAMPTZ NOT NULL,
    is_used BOOLEAN DEFAULT FALSE
);

CREATE INDEX idx_sms_phone ON sms_verifications(phone);

-- ======================
-- 11. User devices (push tokens etc)
-- ======================
CREATE TABLE user_devices (
    id SERIAL PRIMARY KEY,
    user_id INT NOT NULL,
    device_uuid VARCHAR(100) NOT NULL,
    device_type VARCHAR(20) CHECK (device_type IN ('android','ios','web')) DEFAULT 'android',
    push_token VARCHAR(255),
    last_active TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

CREATE INDEX idx_user_devices_user ON user_devices(user_id);

-- ======================
-- 12. User public keys (E2EE)
-- ======================
CREATE TABLE user_keys (
    id SERIAL PRIMARY KEY,
    user_id INT NOT NULL UNIQUE,
    public_key TEXT NOT NULL,         -- base64 / PEM
    key_type VARCHAR(50) DEFAULT 'x25519',
    created_at TIMESTAMPTZ DEFAULT now(),
    last_updated TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- ======================
-- 13. Session keys: encrypted session key per chat per recipient
-- ======================
CREATE TABLE session_keys (
    id SERIAL PRIMARY KEY,
    chat_id INT NOT NULL,
    recipient_user_id INT NOT NULL,            -- user who can decrypt this
    encrypted_session_key TEXT NOT NULL,       -- base64 ciphertext of session key
    encryption_scheme VARCHAR(100) DEFAULT 'X25519+HKDF+ChaCha20-Poly1305',
    created_at TIMESTAMPTZ DEFAULT now(),
    UNIQUE (chat_id, recipient_user_id),
    FOREIGN KEY (recipient_user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (chat_id) REFERENCES chats(id) ON DELETE CASCADE
);

CREATE INDEX idx_session_keys_chat ON session_keys(chat_id);

-- ======================
-- 14. Admin keys (public keys for admin accounts)
-- ======================
CREATE TABLE admin_keys (
    id SERIAL PRIMARY KEY,
    admin_user_id INT NOT NULL,    -- user record that represents the admin operator
    public_key TEXT NOT NULL,
    description VARCHAR(200),
    created_at TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (admin_user_id) REFERENCES users(id) ON DELETE CASCADE
);

-- ======================
-- 15. Admin session keys (session key encrypted for admin)
-- ======================
CREATE TABLE admin_session_keys (
    id SERIAL PRIMARY KEY,
    chat_id INT NOT NULL,
    admin_key_id INT NOT NULL,         -- which admin public key was used
    encrypted_session_key TEXT NOT NULL,
    created_at TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (admin_key_id) REFERENCES admin_keys(id) ON DELETE CASCADE,
    FOREIGN KEY (chat_id) REFERENCES chats(id) ON DELETE CASCADE
);

CREATE INDEX idx_admin_session_keys_chat ON admin_session_keys(chat_id);

-- ======================
-- 16. Key audit log (who accessed/decrypted admin key)
-- ======================
CREATE TABLE key_audit_log (
    id SERIAL PRIMARY KEY,
    actor_user_id INT NOT NULL,   -- who requested/used (admin operator)
    admin_key_id INT NOT NULL,
    chat_id INT NOT NULL,
    action VARCHAR(50) NOT NULL,  -- 'REQUEST_KEY','DECRYPT_MESSAGES','ROTATE_KEY', etc
    reason TEXT,
    details JSONB,
    created_at TIMESTAMPTZ DEFAULT now(),
    FOREIGN KEY (actor_user_id) REFERENCES users(id) ON DELETE CASCADE,
    FOREIGN KEY (admin_key_id) REFERENCES admin_keys(id) ON DELETE CASCADE
);

CREATE INDEX idx_key_audit_chat ON key_audit_log(chat_id);

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
