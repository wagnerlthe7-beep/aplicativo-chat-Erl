# =============================================
# Script para iniciar o servidor com variaveis de ambiente
# Carrega automaticamente do ficheiro .env
# =============================================

Write-Host "[INFO] Iniciando Chat Server..."
Write-Host ""

# Caminho do ficheiro .env (na pasta server)
$envFile = "$PSScriptRoot\.env"

# Verificar se o ficheiro .env existe
if (Test-Path $envFile) {
    Write-Host "[INFO] Carregando variaveis do ficheiro .env..."
    
    # Ler e processar cada linha do .env
    Get-Content $envFile | ForEach-Object {
        # Ignorar linhas vazias e comentarios
        if ($_ -and $_ -notmatch '^\s*#' -and $_ -match '=') {
            $parts = $_ -split '=', 2
            $name = $parts[0].Trim()
            $value = $parts[1].Trim()
            
            # Remover aspas se existirem
            $value = $value -replace '^["'']|["'']$', ''
            
            # Definir variavel de ambiente
            [Environment]::SetEnvironmentVariable($name, $value, "Process")
            Write-Host "   [OK] $name configurado"
        }
    }
    Write-Host ""
} else {
    Write-Host "[AVISO] Ficheiro .env nao encontrado em: $envFile"
    Write-Host "        A criar ficheiro .env de exemplo..."
    
    # Criar ficheiro .env de exemplo
    $envContent = @"
# Database Configuration
DB_HOST=localhost
DB_PORT=5432
DB_NAME=chat_app_db
DB_USER=postgres
DB_PASS=your_password_here

# JWT Secret
SESSION_SECRET=your_jwt_secret_here

# Firebase Configuration
FIREBASE_API_KEY=your_firebase_api_key_here

# FCM Push Notifications
GOOGLE_APPLICATION_CREDENTIALS=E:\aplicativo-chat\server\config\firebase-service-account.json

# Server Configuration
PORT=4000
"@
    $envContent | Out-File -FilePath $envFile -Encoding UTF8
    
    Write-Host "   [OK] Ficheiro .env criado!"
    Write-Host "   [AVISO] EDITA o ficheiro .env com as tuas credenciais antes de continuar!"
    Write-Host ""
    Write-Host "   Localizacao: $envFile"
    Write-Host ""
    exit 1
}

# Verificar variaveis obrigatorias
$required = @("DB_HOST", "DB_NAME", "DB_USER", "DB_PASS", "SESSION_SECRET")
$missing = @()

foreach ($var in $required) {
    if (-not [Environment]::GetEnvironmentVariable($var)) {
        $missing += $var
    }
}

if ($missing.Count -gt 0) {
    Write-Host "[ERRO] Variaveis obrigatorias em falta no .env:"
    foreach ($var in $missing) {
        Write-Host "   - $var"
    }
    Write-Host ""
    exit 1
}

Write-Host "[OK] Todas as variaveis de ambiente configuradas!"
Write-Host ""

# Verificar se o Service Account existe (para FCM)
$fcmCredentials = [Environment]::GetEnvironmentVariable("GOOGLE_APPLICATION_CREDENTIALS")
if ($fcmCredentials) {
    if (Test-Path $fcmCredentials) {
        Write-Host "[OK] FCM Service Account encontrado: $fcmCredentials"
    } else {
        Write-Host "[AVISO] FCM Service Account NAO encontrado: $fcmCredentials"
        Write-Host "        Push notifications nao funcionarao ate configurar!"
    }
}
Write-Host ""

# Mudar para o directorio chat_app
Write-Host "[INFO] Mudando para directorio: $PSScriptRoot\apps\chat_app"
Set-Location "$PSScriptRoot\apps\chat_app"

# Iniciar o servidor com o nome do node
Write-Host "[INFO] Iniciando servidor Erlang..."
Write-Host "============================================="
Write-Host ""

rebar3 shell --name chat@localhost
