# syntax=docker/dockerfile:1
# Multi-stage build for lambda-cek web UI (Linux, .NET 8)
# Build stage
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /workspace

# Copy entire repository into isolated workspace (avoid clobbering system dirs like /bin)
COPY . .

# Force library to single target net8.0 (rewrite multi-target line) to avoid needing .NET 9 SDK inside container
RUN sed -i 's#<TargetFrameworks>net8.0;net9.0</TargetFrameworks>#<TargetFrameworks>net8.0</TargetFrameworks>#' src/lambda-cek.csproj || true

# Restore & publish
RUN dotnet restore src-webui/lambda-cek.webui.csproj \
 && dotnet publish src-webui/lambda-cek.webui.csproj -c Release -f net10.0 -o /app/publish -p:UseAppHost=false

# Runtime image (Alpine for small footprint)
FROM mcr.microsoft.com/dotnet/aspnet:10.0-alpine AS final
WORKDIR /app

# Base ASP.NET 8 alpine image already uses a non-root 'app' user; no need to recreate

ENV ASPNETCORE_URLS=http://0.0.0.0:8080 \
    DOTNET_EnableDiagnostics=0

EXPOSE 8080

# Copy published output
COPY --from=build /app/publish .

# Healthcheck hitting the API health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 CMD wget -qO- http://127.0.0.1:8080/api/health || exit 1

ENTRYPOINT ["dotnet","lambda-cek.webui.dll"]
