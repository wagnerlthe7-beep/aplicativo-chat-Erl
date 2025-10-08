# Test script for revoke others endpoint
$accessToken = "eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9.eyJ1c2VyX2lkIjoiMTIzIiwicGhvbmUiOiIrMTIzNDU2Nzg5MCIsIm5hbWUiOiJUZXN0IFVzZXIiLCJpYXQiOjE3MzU2NzI4MDAsImV4cCI6MTczNTY3NjQwMCwic2Vzc2lvbl9pZCI6IjQ1NiJ9.test_signature"
$deviceUuid = "test-device-uuid-123"

$body = @{
    access_token = $accessToken
    device_uuid = $deviceUuid
} | ConvertTo-Json

Write-Host "Testing revoke others endpoint..."
Write-Host "Body: $body"

try {
    $response = Invoke-WebRequest -Uri "http://localhost:4000/auth/revoke-others" -Method POST -Headers @{"Content-Type"="application/json"} -Body $body
    Write-Host "Status: $($response.StatusCode)"
    Write-Host "Response: $($response.Content)"
} catch {
    Write-Host "Error: $($_.Exception.Message)"
    Write-Host "Response: $($_.Exception.Response)"
}
