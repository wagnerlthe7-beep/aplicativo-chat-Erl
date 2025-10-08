Write-Host "🧪 Testando endpoint /auth/revoke-others..."

$testPayload = @{
    access_token = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoiMTIzIiwicGhvbmUiOiIrMTIzNDU2Nzg5MCIsIm5hbWUiOiJUZXN0IFVzZXIiLCJpYXQiOjE3MzU2NzI4MDAsImV4cCI6MTczNTY3NjQwMCwic2Vzc2lvbl9pZCI6IjQ1NiJ9.test_signature"
    device_uuid = "test-device-uuid-123"
} | ConvertTo-Json

Write-Host "📤 Payload: $testPayload"

try {
    $response = Invoke-RestMethod -Uri "http://localhost:4000/auth/revoke-others" -Method POST -ContentType "application/json" -Body $testPayload
    Write-Host "✅ Sucesso: $response"
} catch {
    Write-Host "❌ Erro: $($_.Exception.Message)"
    if ($_.Exception.Response) {
        $statusCode = $_.Exception.Response.StatusCode
        Write-Host "📊 Status: $statusCode"
    }
}
