# Teste direto do endpoint revoke-others
Write-Host "🧪 Testando endpoint /auth/revoke-others..."

# Simular exatamente o que o Flutter envia
$testPayload = @{
    access_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoiMTIzIiwicGhvbmUiOiIrMTIzNDU2Nzg5MCIsIm5hbWUiOiJUZXN0IFVzZXIiLCJpYXQiOjE3MzU2NzI4MDAsImV4cCI6MTczNTY3NjQwMCwic2Vzc2lvbl9pZCI6IjQ1NiJ9.test_signature"
    device_uuid = "test-device-uuid-123"
} | ConvertTo-Json

Write-Host "📤 Payload: $testPayload"

try {
    $response = Invoke-RestMethod -Uri "http://localhost:4000/auth/revoke-others" -Method POST -ContentType "application/json" -Body $testPayload
    Write-Host "✅ Sucesso: $response"
} catch {
    Write-Host "❌ Erro HTTP: $($_.Exception.Message)"
    
    # Tentar obter mais detalhes do erro
    if ($_.Exception.Response) {
        $statusCode = $_.Exception.Response.StatusCode
        Write-Host "📊 Status Code: $statusCode"
        
        try {
            $stream = $_.Exception.Response.GetResponseStream()
            $reader = New-Object System.IO.StreamReader($stream)
            $responseBody = $reader.ReadToEnd()
            Write-Host "📄 Response Body: $responseBody"
        } catch {
            Write-Host "❌ Não foi possível ler o corpo da resposta"
        }
    }
}
