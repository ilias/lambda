Param(
    [switch]$SkipBuild
)

Write-Host "[smoke] Starting smoke tests" -ForegroundColor Cyan

if (-not $SkipBuild) {
    Write-Host "[smoke] Building solution" -ForegroundColor DarkCyan
    dotnet build | Out-Null
}

$cli = "src-cli/lambda-cek.cli.csproj"

function Invoke-LambdaScript {
    param([string]$File)
    Write-Host "[smoke] Running $File" -ForegroundColor DarkGray
    dotnet run --project $cli -- --no-repl $File | Write-Host
}

# Core discovery & docs smoke
Invoke-LambdaScript "tests/sprint1-smoke.lambda"

# Module export/hide smoke
Invoke-LambdaScript "tests/module-export-hide.lambda"
Invoke-LambdaScript "tests/module-export-hide-smoke.lambda"

Write-Host "[smoke] Completed" -ForegroundColor Green