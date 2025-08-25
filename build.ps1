Param(
	[switch]$NoBuild
)

$ErrorActionPreference = 'Stop'

if(-not $NoBuild){
	Write-Host "[build] Compiling projects (Release)" -ForegroundColor Cyan
	dotnet build --configuration Release
} else {
	Write-Host "[build] Skipping dotnet build (NoBuild flag)." -ForegroundColor Yellow
}

$name = 'lambda-cek-webui'
$altName = 'lambda-webui'

$containers = @(docker ps -aq -f "name=^(${name}|${altName})$" 2>$null)
if ($containers.Count -gt 0) {
    Write-Host "[docker] Removing existing container(s): $containers" -ForegroundColor Yellow
    foreach ($c in $containers) {
        docker rm -f $c | Out-Null
    }
} else {
    Write-Host "[docker] No existing running/stopped container named '$name' or '$altName'" -ForegroundColor DarkGray
}

Write-Host "[docker] Checking for existing image '$name'" -ForegroundColor Cyan
$existingImage = docker images -q $name 2>$null
if($existingImage){
	Write-Host "[docker] Removing existing image $existingImage" -ForegroundColor Yellow
	docker rmi -f $existingImage | Out-Null
} else {
	Write-Host "[docker] No existing image to remove" -ForegroundColor DarkGray
}

Write-Host "[docker] Building fresh image '$name'" -ForegroundColor Cyan
docker build -t $name -f src-webui/Dockerfile .

# run container
# docker run -d -p 8080:8080 --name lambda-cek-webui lambda-cek-webui
