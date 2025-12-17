-- populate_test_data.sql
INSERT INTO users (id, name, phone, is_verified, is_active, created_at) 
VALUES 
(123, 'Test User 123', '+244900000000', true, true, NOW()),
(456, 'Test User 456', '+244900000001', true, true, NOW())
ON CONFLICT (id) DO NOTHING;

-- Inserir roles básicos se não existirem
INSERT INTO roles (id, name, description) VALUES
(1, 'User', 'Regular user'),
(2, 'Admin', 'Administrator')
ON CONFLICT (id) DO NOTHING;

-- Associar usuários às roles
INSERT INTO user_roles (user_id, role_id) VALUES
(123, 1),
(456, 1)
ON CONFLICT (user_id, role_id) DO NOTHING;