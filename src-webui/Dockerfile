# syntax=docker/dockerfile:1
# Multi-stage build for lambda-cek web UI (Linux, .NET 10)

# Build stage
FROM mcr.microsoft.com/dotnet/sdk:10.0 AS build
WORKDIR /src

# Copy project files first for better layer caching
COPY src/lambda-cek.csproj src/
COPY src-webui/lambda-cek.webui.csproj src-webui/

# Restore dependencies for the web UI (pulls referenced library too)
RUN dotnet restore src-webui/lambda-cek.webui.csproj

# Copy the rest of the repository
COPY . .

# Publish the web UI for .NET 10 (perform a full restore in this stage to avoid host-specific NuGet fallbacks)
RUN dotnet publish src-webui/lambda-cek.webui.csproj -c Release -f net10.0 -o /app/publish -p:UseAppHost=false

# Runtime image (Alpine for small footprint)
FROM mcr.microsoft.com/dotnet/aspnet:10.0-alpine AS final
WORKDIR /app

# Environment
ENV ASPNETCORE_URLS=http://0.0.0.0:8080 \
    DOTNET_EnableDiagnostics=0

EXPOSE 8080

# Copy published output
COPY --from=build /app/publish .

# Healthcheck hitting the API health endpoint
HEALTHCHECK --interval=30s --timeout=5s --start-period=20s --retries=3 CMD wget -qO- http://127.0.0.1:8080/api/health || exit 1

ENTRYPOINT ["dotnet","lambda-cek.webui.dll"]
