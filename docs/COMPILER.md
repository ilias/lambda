# Build, Packaging & Embedding Guide

This document consolidates build instructions, project layout, distribution targets, deployment options, and programmatic embedding guidance.

Sections

- Project Layout
- Prerequisites
- Building & Running (CLI, Web API, Web UI)
- Unified Build Script & Documentation Generation
- Docker (Web UI) & Container Notes
- NuGet Packaging & Versioning
- Embedding API (C#) & Native Primitives
- Multi‑user / Deployment Strategies
- Performance Modes & Recommendations
- Troubleshooting
- Streaming Modes (SSE vs WebSocket vs Buffered)

---

## Project Layout

| Project | Type | Path | Purpose |
|---------|------|------|---------|
| `lambda-cek` | Class Library (`net8.0;net9.0`) | `src/` | Core interpreter (parser, CEK, macros, natives) |
| `lambda-cek.cli` | Console App | `src-cli/` | Interactive REPL / file runner |
| `lambda-cek.web` | Minimal Web API | `src-web/` | HTTP endpoints (eval/load/health) |
| `lambda-cek.webui` | ASP.NET Core + Static UI | `src-webui/` | Browser REPL + streaming logs |

## Prerequisites

- .NET 8 SDK (optionally .NET 9 preview for multi-target build)
- PowerShell (build script examples assume Windows pwsh)
- (Optional) Docker Desktop for container builds
- (Optional) Pandoc for HTML doc generation displayed in Web UI

docker version     # optional
docker version     # optional
Verify tools:

```powershell
dotnet --version
pandoc --version   # optional
docker version     # optional
```

---

## Building & Running

Build all:

```powershell
dotnet build
```

CLI REPL:

```powershell
dotnet run --project src-cli/lambda-cek.cli.csproj
dotnet run --project src-cli/lambda-cek.cli.csproj -- myfile.lambda tests.lambda
```

### Non-Interactive Script Mode (`--no-repl`)

For CI, smoke tests, or batch processing you can run the CLI without entering the interactive REPL using `--no-repl`. Supply one or more `.lambda` files; `stdlib.lambda` is loaded automatically first (if present), then each file is executed in order, and the process exits.

```powershell
# Single script
dotnet run --project src-cli/lambda-cek.cli.csproj -- --no-repl tests/sprint1-smoke.lambda

# Multiple scripts (executed in order)
dotnet run --project src-cli/lambda-cek.cli.csproj -- --no-repl tests/module-export-hide.lambda tests/module-export-hide-smoke.lambda
```

Behaviors & Notes:

- Missing files print `File not found:` but do not abort subsequent files.
- Suppresses the interactive banner and prompt.
- Combine commands inside a file with `;` (e.g., `:doc export docs/generated-symbols.md; :stats`).
- Useful in pipelines: documentation export, regression smoke runs, or reproducible examples.

Minimal scripted doc export example:

```lambda
# script.lambda
:doc export artifacts/docs.md
```

Run with:

```powershell
dotnet run --project src-cli/lambda-cek.cli.csproj -- --no-repl script.lambda
```

Web API:

```powershell
dotnet run --project src-web/lambda-cek.web.csproj
# Override port
dotnet run --project src-web/lambda-cek.web.csproj -- --urls http://localhost:5055
```

Web UI:

```powershell
dotnet run --project src-webui/lambda-cek.webui.csproj
```

Navigate to the printed URL (typically <http://localhost:5000>). Toggle streaming / WebSocket in UI.

## Streaming Modes (Comparison)

| Mode | Enable | Transport | Log Delivery | Response JSON | Best For |
|------|--------|-----------|--------------|---------------|---------|
| Buffered | Streaming Off | none | After completion (batched) | `{ output, normalized, logs[] }` | Simplicity, full log capture |
| SSE | Streaming On | `/api/stream` | Incremental (auto‑reconnect) | `{ output, normalized }` | Long running eval, progressive feedback |
| WebSocket | Streaming On + WS On | `/ws` | Incremental (manual reconnect) | `{ output, normalized }` | Future interactive features |

Notes:

1. Toggling streaming clears the UI log to delineate sessions.
2. SSE auto‑reconnects with capped backoff; failures surface a status banner.
3. WebSocket is optional; fallback gracefully to SSE if handshake fails.
4. All modes share a single interpreter instance & `Logger`; only transport differs.

API endpoints:

| Method | Route | Purpose |
|--------|-------|---------|
| GET | `/api/eval?expr=...` | Evaluate one expression (buffered logs if streaming off) |
| POST | `/api/load` | Load `.lambda` file (returns message + logs when buffered) |
| GET | `/api/stream` | SSE log stream (one `data:` line per log) |
| GET | `/api/health` | Health check |
| WS | `/ws` | WebSocket log frames |

Implementation tips:

- For multi‑tenant hosting, allocate an interpreter per session or workspace to avoid cross‑user state leakage.
- Keep log lines single line (SSE collapses newlines) and concise.
- When streaming is disabled, logs are returned in the evaluation JSON response (`logs[]`).
- Use `Logger.Subscribe` in custom hosts to forward events to external telemetry.

Disabling streaming uses a buffered snapshot between `Logger.ClearBuffer()` and evaluation completion for parity with CLI behavior.

---

## Unified Build Script (`build.ps1`)

Common flags:

| Flag | Effect |
|------|--------|
| `-Pack` | Builds & packs NuGet (`artifacts/`) |
| `-Validate` | Validates package content (fail fast) |
| `-SkipPackOnError` | Continue even if validation failed |
| `-NoDocker` | Skip Docker image build |

Example:

```powershell
./build.ps1 -Pack -Validate
```

### Documentation Generation (Pandoc)

If Pandoc is on PATH, `README.md` → `readme.html` + `readme.css` copied into `src-webui/wwwroot/` for in-browser docs.

Install (examples):

```powershell
winget install --id JohnMacFarlane.Pandoc -e
choco install pandoc
```

---

## Docker (Web UI)

Build image:

```powershell
docker build -t lambda-cek-webui -f src-webui/Dockerfile .
```

Run:

```powershell
docker run --rm -p 8080:8080 --name lambda-cek lambda-cek-webui
```

Bind mount repo (read-only) for dynamic file loading:

```powershell
docker run --rm -p 8080:8080 -v $PWD:/data:ro --name lambda-cek lambda-cek-webui
```

Custom port:

```powershell
docker run --rm -e ASPNETCORE_URLS=http://0.0.0.0:5005 -p 5005:5005 lambda-cek-webui
```

Security considerations:

- No auth / sandbox by default; add reverse proxy, timeouts, CPU/memory limits for multi-user / public use.
- Long evaluations can monopolize interpreter; wrap with cancellation tokens in host.

---

## NuGet Packaging

Pack core library:

```powershell
dotnet pack src/lambda-cek.csproj -c Release -o artifacts
```

Override version:

```powershell
dotnet pack src/lambda-cek.csproj -c Release -o artifacts /p:PackageVersion=0.1.1
```

Consume:

```powershell
dotnet add package LambdaCalculus.Interpreter --version 0.1.0
```

Planned: CI automation (GitHub Actions) to pack & push on tagged release.

---

## Embedding API (C#)

Minimal:

```csharp
var logger = new LambdaCalculus.Logger { EnableBuffering = true };
var interp = new LambdaCalculus.Interpreter(logger: logger);
await interp.LoadFileIfExistsAsync("stdlib.lambda");
var (_, value) = await interp.ProcessInputAsync("succ 41");
Console.WriteLine(interp.Format(value)); // 42
```

Register a native primitive:

```csharp
interp.RegisterNativeFunction("triple", (op, args, env) =>
{
    if (args.Count == 1 && interp.TryGetChurchInt(args[0], env, out var n))
        return interp.MakeChurchNumeral(n * 3);
    return null; // fallback to pure path
});
```

Batch process commands + expression:

```csharp
await interp.ProcessInputAsync(":lazy off; :native on; plus 20 22");
```

Buffered logs:

```csharp
foreach (var line in logger.GetBufferSnapshot()) Console.WriteLine(line);
```

---

## Native Arithmetic & List Fast Paths

Enabled with `:native on`. Accelerated operations: `plus minus mult div mod succ pred iszero lt leq eq geq gt neq max min sqrt` plus list helpers (`map filter length append reverse take drop any all find sum product`). Disable with `:native off` for purist or benchmarking scenarios. Structural equivalence helpers (`alphaEq betaEq hashEq etaEq`) are always available.

Guidelines:

- Use natives for performance demonstrations or large numeric/list workloads.
- Disable when validating pure lambda reductions for teaching.

---

## Multi‑user / Deployment Strategies

| Strategy | Isolation | Pros | Cons |
|----------|-----------|------|------|
| Per-request instance | Full | No shared state | High allocation cost |
| Session-scoped (dictionary) | Medium | Persistent user context | State cleanup complexity |
| Interpreter pool | Medium | Warm reuse | Risk of residual state if not fully cleared |
| Stateless eval (single expr) | Logical | Horizontal scale, safe | No interactive definitions |

Hardening checklist:

- Enforce iteration / time limits (cancellation tokens).
- Restrict file loading paths; sanitize inputs.
- Cap macro clause count & expansion depth.
- Add authentication & rate limiting on public endpoints.

Observability: subscribe to logger (`Logger.Subscribe`) and forward lines asynchronously; avoid blocking evaluation path.

---

## Performance Modes

| Scenario | Recommended Settings |
|----------|----------------------|
| Benchmark pure lambda | `:lazy off; :native off; :pretty off` |
| Demonstrate laziness | `:lazy on; take 10 [0 .. 1000000]` |
| Maximize cache hits | Repeated same call then `:stats` |
| Audit macro expansion | `:step on; :pretty off` (temporarily) |

Key metrics: `Iterations`, `CacheHits/Misses`, `TimeInEvaluation`, `ThunkForceCount`.

---

## Troubleshooting

| Issue | Cause | Fix |
|-------|-------|-----|
| NU5019 README not found | Pack path mismatch | Confirm relative README path in `.csproj` |
| NETSDK1057 preview warning | net9.0 preview | Ignore or drop net9.0 target |
| HTTPS trust prompt | Dev cert untrusted | `dotnet dev-certs https --trust` |
| Port conflict | Port in use | Override with `--urls` |
| Excessive iterations | Non-terminating recursion | Add base case or raise `:depth` temporarily |
| Slow macro expansion | Large structural patterns + guards | Refactor macros / limit depth |

---

## Notes for Contributors: Unquote Parsing vs. QQ Walker

Rationale for parsing `~`/`~@` as primaries (2025‑10):

- Parser responsibility: keep unquotes atomic. The parser consumes exactly one primary after `~` and `~@` so forms like `qq (plus ~x 1)` pass `x` as a single argument rather than greedily absorbing `1` (which would happen if a full expression were parsed).
- Expander responsibility: flattening. The quasiquote walker handles argument/list spine reconstruction and `~@` splicing (for both native list literals and Church lists). This separation avoids ambiguity and preserves intuitive argument boundaries.

Where to look:

- `src/Parser.Partials.Expressions.cs`: `ParseUnquote` and `ParseUnquoteSplice` use `ParsePrimary` to bind to a single atom.
- `src/Parser.Macros.cs`: quasiquote walker and splice handling that rebuild application/list spines and applies `~@` flattening.

Practical guidance:

- Use `~expr` to inject one argument/element and `~@listExpr` to splice multiple.
- To unquote a compound site expression, wrap it: `~(f x)`.
- Tests around “Quasiquote / Unquote” in `tests.lambda` exercise these semantics; keep them green when modifying parser or macro expander.

---

## Quick Commands (Cheat)

| Target | Command |
|--------|---------|
| Build all | `dotnet build -c Release` |
| Pack NuGet | `dotnet pack src/lambda-cek.csproj -c Release -o artifacts` |
| CLI REPL | `dotnet run --project src-cli/lambda-cek.cli.csproj` |
| Web UI | `dotnet run --project src-webui/lambda-cek.webui.csproj` |
| Web API | `dotnet run --project src-web/lambda-cek.web.csproj` |
| Docker image | `docker build -t lambda-cek-webui -f src-webui/Dockerfile .` |

See `LANGUAGE.md` for syntax / macros and `COMMANDS.md` for interactive reference.
