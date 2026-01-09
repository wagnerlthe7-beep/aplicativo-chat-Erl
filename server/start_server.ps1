# Script para iniciar o servidor com variáveis de ambiente
# PowerShell script para iniciar o chat server

Write-Host "🚀 Iniciando Chat Server com variáveis de ambiente..."

# Setar variáveis de ambiente
$env:FIREBASE_API_KEY="sua_chave_firebase_aqui"
$env:SESSION_SECRET="seu_secreto_de_sessao_aqui"
$env:DB_HOST="localhost"
$env:DB_NAME="chat_app_db"
$env:DB_USER="postgres"
$env:DB_PASS="sua_senha_postgres"
$env:DB_PORT="5432"

Write-Host "✅ Variáveis de ambiente configuradas"
Write-Host "📍 Diretório: e:\aplicativo-chat\server\apps\chat_app"

# Mudar para o diretório correto
Set-Location "e:\aplicativo-chat\server\apps\chat_app"

# Iniciar o servidor
Write-Host "🔥 Iniciando servidor Erlang..."
rebar3 shell
