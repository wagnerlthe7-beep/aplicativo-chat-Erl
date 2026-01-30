-- Migration: Add unique constraint to user_devices table
-- This is needed for the FCM token registration system
-- 
-- Run this migration on your database before deploying the new FCM features

-- Add unique constraint on (user_id, device_uuid) if not exists
-- This allows upsert operations when registering FCM tokens
DO $$
BEGIN
    IF NOT EXISTS (
        SELECT 1 FROM pg_constraint 
        WHERE conname = 'user_devices_user_device_unique'
    ) THEN
        ALTER TABLE user_devices 
        ADD CONSTRAINT user_devices_user_device_unique 
        UNIQUE (user_id, device_uuid);
    END IF;
END $$;

-- Add index on push_token for faster lookups
CREATE INDEX IF NOT EXISTS idx_user_devices_push_token ON user_devices(push_token);

-- Add index on user_id for faster lookups
CREATE INDEX IF NOT EXISTS idx_user_devices_user_id ON user_devices(user_id);

-- Optional: Clean up old/duplicate tokens
-- DELETE FROM user_devices WHERE push_token IS NULL OR push_token = '';

