# Teste simples do endpoint revoke-others
Write-Host "🧪 Testando endpoint /auth/revoke-others..."

# Dados de teste
$testData = @{
    access_token = "test_token_123"
    device_uuid = "test_device_456"
} | ConvertTo-Json

Write-Host "📤 Enviando dados: $testData"

try {
    $response = Invoke-RestMethod -Uri "http://localhost:4000/auth/revoke-others" -Method POST -ContentType "application/json" -Body $testData
    Write-Host "✅ Resposta: $response"
} catch {
    Write-Host "❌ Erro: $($_.Exception.Message)"
    if ($_.Exception.Response) {
        $reader = New-Object System.IO.StreamReader($_.Exception.Response.GetResponseStream())
        $responseBody = $reader.ReadToEnd()
        Write-Host "📄 Corpo da resposta: $responseBody"
    }
}

