Param(
	[switch]$NoBuild,
	[switch]$Pack,            # dotnet pack core library
	[string]$Version,         # optional override package version
	[switch]$NoDocker,        # skip docker image build
	[switch]$CleanArtifacts,  # remove artifacts folder first
	[switch]$Validate,        # validate presence of package content files
	[switch]$SkipPackOnError, # if validation fails, skip pack instead of stopping
	[string]$Output = 'artifacts'
)

$ErrorActionPreference = 'Stop'

if ($CleanArtifacts -and (Test-Path $Output)) {
	Write-Host "[clean] Removing '$Output'" -ForegroundColor Yellow
	Remove-Item -Recurse -Force $Output
}

if (-not $NoBuild) {
	Write-Host "[build] Restoring & compiling solution (Release)" -ForegroundColor Cyan
	dotnet restore | Out-Null
	dotnet build lambda-cek.sln --configuration Release --no-restore
}
else {
	Write-Host "[build] Skipping dotnet build (NoBuild flag)." -ForegroundColor Yellow
}

Write-Host "[readme] Generating styled readme.html (pandoc)." -ForegroundColor Cyan
if (Test-Path README.md) {
	$cssPath = Join-Path $PSScriptRoot 'readme.css'
	if (-not (Test-Path $cssPath)) {
		Write-Host "[readme][warn] readme.css not found; proceeding without custom stylesheet." -ForegroundColor Yellow
	}
	$pandocAvailable = Get-Command pandoc -ErrorAction SilentlyContinue
	if (-not $pandocAvailable) {
		Write-Host "[readme][error] pandoc not found on PATH. Install pandoc to generate readme.html." -ForegroundColor Red
	} else {
		try {
			$pandocArgs = @(
				'README.md',
				'--from','gfm',
				'--standalone',
				'--toc','--toc-depth=3',
				'--metadata','title=Lambda Calculus Interpreter',
				'--metadata','lang=en',
				'--highlight-style','pygments',
				'--embed-resources',
				'--section-divs',
				'-V','viewport=width=device-width,initial-scale=1',
				'--output','readme.html'
			)
			if (Test-Path $cssPath) { $pandocArgs += @('--css','readme.css') }
			pandoc @pandocArgs
			if (Test-Path 'src-webui/wwwroot') {
				Copy-Item readme.html src-webui/wwwroot/readme.html -Force
				if (Test-Path $cssPath) { Copy-Item $cssPath src-webui/wwwroot/readme.css -Force }
				Write-Host "[readme] Copied readme.html (and css if present) to web UI wwwroot." -ForegroundColor DarkCyan
			}
		}
		catch {
			Write-Host "[readme][warn] pandoc error: $($_.Exception.Message)" -ForegroundColor Yellow
		}
	}
} else {
	Write-Host "[readme][warn] README.md not found; skipping HTML generation." -ForegroundColor Yellow
}

if ($Pack) {
	if (-not (Test-Path $Output)) { New-Item -ItemType Directory -Path $Output | Out-Null }

	# Validation of expected files for pack (README, stdlib, tests)
	$expected = @(
		'README.md',
		'stdlib.lambda',
		'tests.lambda'
	)
	$missing = @()
	foreach ($f in $expected) { if (-not (Test-Path $f)) { $missing += $f } }

	if ($missing.Count -gt 0) {
		Write-Host "[pack][warn] Missing expected files at repo root: $($missing -join ', ')" -ForegroundColor Yellow
		if (-not $SkipPackOnError) {
			Write-Host "[pack][error] Aborting pack due to missing files (use -SkipPackOnError to continue)." -ForegroundColor Red
			exit 1
		} else {
			Write-Host "[pack] Continuing despite missing files (SkipPackOnError)." -ForegroundColor DarkYellow
		}
	}

	# Construct pack args
	$packArgs = @('pack', 'src/lambda-cek.csproj', '-c', 'Release', '-o', $Output, '--no-build')
	if ($Version) { $packArgs += "/p:PackageVersion=$Version" }
	Write-Host "[pack] dotnet $($packArgs -join ' ')" -ForegroundColor Cyan
	dotnet @packArgs
}

if (-not $NoDocker) {
	$name = 'lambda-cek-webui'
	$altName = 'lambda-webui'

	Write-Host "[docker] Preparing image build" -ForegroundColor Cyan
	$containers = @(docker ps -aq -f "name=^(${name}|${altName})$" 2>$null)
	if ($containers.Count -gt 0) {
		Write-Host "[docker] Removing existing container(s): $containers" -ForegroundColor Yellow
		foreach ($c in $containers) { docker rm -f $c | Out-Null }
	} else {
		Write-Host "[docker] No existing container(s)" -ForegroundColor DarkGray
	}

	$existingImage = docker images -q $name 2>$null
	if ($existingImage) {
		Write-Host "[docker] Removing existing image $existingImage" -ForegroundColor Yellow
		docker rmi -f $existingImage | Out-Null
	} else {
		Write-Host "[docker] No existing image to remove" -ForegroundColor DarkGray
	}

	Write-Host "[docker] Building fresh image '$name'" -ForegroundColor Cyan
	docker build -t $name -f src-webui/Dockerfile .
} else {
	Write-Host "[docker] Skipped (NoDocker flag)." -ForegroundColor Yellow
}

Write-Host "[done] Build pipeline completed." -ForegroundColor Green

# Quick Reference Summary
# CLI: dotnet run --project src-cli
# Web API: dotnet run --project src-web
# Web UI: dotnet run --project src-webui
# Pack: dotnet pack src/lambda-cek.csproj -c Release -o artifacts
# All-in-one: .\build.ps1 -Pack (add -NoDocker if you don’t need the image)
# Validate only: .\build.ps1 -Pack -Validate -SkipPackOnError