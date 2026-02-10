# Deploying quickapi Applications

quickapi apps are standard Common Lisp programs that listen on a TCP port. The executable is a single binary produced by `sb-ext:save-lisp-and-die`. Deployment boils down to: get the binary running, keep it running, and put a reverse proxy in front.

## Table of Contents

- [Building the Executable](#building-the-executable)
- [Strategy 1: Systemd (GNU/Linux)](#strategy-1-systemd-gnulinux)
- [Strategy 2: Docker](#strategy-2-docker)
- [Strategy 3: launchd (macOS)](#strategy-3-launchd-macos)
- [Strategy 4: Bare Process with nohup](#strategy-4-bare-process-with-nohup)
- [Reverse Proxy](#reverse-proxy)
- [Agent-Driven Deployment](#agent-driven-deployment)
- [Environment Variables](#environment-variables)
- [Health Checks](#health-checks)
- [Graceful Shutdown](#graceful-shutdown)

---

## Building the Executable

```bash
make build
```

This produces a compressed standalone binary (e.g., `./my-api`) via `sb-ext:save-lisp-and-die`. The binary contains the full Lisp image — no SBCL or Quicklisp installation needed at runtime.

Test it locally before deploying:

```bash
PORT=8000 ./my-api
curl http://localhost:8000/health
```

---

## Strategy 1: Systemd (GNU/Linux)

**Best for:** Production servers, VPS, bare metal. The recommended approach for GNU/Linux.

quickapi can generate a systemd unit file for you:

```lisp
(quickapi:generate-systemd-unit :api-name "my-api"
                                :description "My API Service"
                                :user "www-data"
                                :working-dir "/opt/my-api"
                                :port 8000)
```

Or generate all deployment files at once:

```lisp
(quickapi:generate-deployment-files :directory "/opt/my-api/"
                                    :system-name "my-api"
                                    :api-name "My API"
                                    :port 8000)
```

### Manual Setup

1. Copy the executable and create the directory structure:

```bash
sudo mkdir -p /opt/my-api/data
sudo cp my-api /opt/my-api/
sudo chown -R www-data:www-data /opt/my-api
```

2. Create `/etc/systemd/system/my-api.service`:

```ini
[Unit]
Description=My API Service
After=network.target
Wants=network-online.target

[Service]
Type=simple
User=www-data
Group=www-data
WorkingDirectory=/opt/my-api

EnvironmentFile=-/opt/my-api/.env
Environment=PORT=8000

ExecStart=/opt/my-api/my-api
Restart=always
RestartSec=5

# Security hardening
NoNewPrivileges=yes
PrivateTmp=yes
ProtectSystem=strict
ProtectHome=yes
ReadWritePaths=/opt/my-api

# Logging
StandardOutput=journal
StandardError=journal
SyslogIdentifier=my-api

[Install]
WantedBy=multi-user.target
```

3. Enable and start:

```bash
sudo systemctl daemon-reload
sudo systemctl enable my-api
sudo systemctl start my-api
```

4. Manage:

```bash
sudo systemctl status my-api     # Check status
sudo systemctl stop my-api       # Stop
sudo systemctl restart my-api    # Restart
journalctl -u my-api -f          # Tail logs
```

---

## Strategy 2: Docker

**Best for:** Consistent environments, orchestration (Compose, Swarm, k8s), CI/CD pipelines.

The template includes a working Dockerfile. It builds the executable inside the container.

### Build and Run

```bash
make docker-build    # docker build -t my-api .
make docker-run      # docker run -p 8000:8000 -v $(PWD)/data:/app/data my-api
```

### Docker Compose

Create `docker-compose.yml`:

```yaml
services:
  api:
    build: .
    ports:
      - "8000:8000"
    volumes:
      - ./data:/app/data
    environment:
      - PORT=8000
      - DB_PATH=/app/data/app.db
      - JWT_SECRET=change-me-in-production
    restart: unless-stopped
    healthcheck:
      test: ["CMD", "curl", "-f", "http://localhost:8000/health"]
      interval: 30s
      timeout: 3s
      retries: 3
```

Manage:

```bash
docker compose up -d       # Start in background
docker compose down        # Stop
docker compose logs -f     # Tail logs
docker compose restart     # Restart
```

### Production Tips

- Mount the SQLite database directory as a volume so data persists across container rebuilds.
- Set `restart: unless-stopped` so the container restarts on crash but not after explicit `docker compose down`.
- The Dockerfile includes a `HEALTHCHECK` directive, which Docker and orchestrators use automatically.

---

## Strategy 3: launchd (macOS)

**Best for:** Running on macOS servers or development machines that should auto-start the API.

Create `~/Library/LaunchAgents/com.example.my-api.plist` (user agent) or `/Library/LaunchDaemons/com.example.my-api.plist` (system daemon):

```xml
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
  "http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>
    <key>Label</key>
    <string>com.example.my-api</string>

    <key>ProgramArguments</key>
    <array>
        <string>/opt/my-api/my-api</string>
    </array>

    <key>WorkingDirectory</key>
    <string>/opt/my-api</string>

    <key>EnvironmentVariables</key>
    <dict>
        <key>PORT</key>
        <string>8000</string>
        <key>DB_PATH</key>
        <string>/opt/my-api/data/app.db</string>
    </dict>

    <key>KeepAlive</key>
    <true/>

    <key>RunAtLoad</key>
    <true/>

    <key>StandardOutPath</key>
    <string>/opt/my-api/logs/stdout.log</string>

    <key>StandardErrorPath</key>
    <string>/opt/my-api/logs/stderr.log</string>
</dict>
</plist>
```

Manage:

```bash
# Load (and start, since RunAtLoad is true)
launchctl load ~/Library/LaunchAgents/com.example.my-api.plist

# Unload (stop)
launchctl unload ~/Library/LaunchAgents/com.example.my-api.plist

# Check status
launchctl list | grep my-api
```

Note: launchd does not rotate logs. Use `newsyslog` or `logrotate` (via Homebrew) for log rotation.

---

## Strategy 4: Bare Process with nohup

**Best for:** Quick testing, temporary deploys, environments where you don't want a service manager.

```bash
nohup ./my-api > my-api.log 2>&1 &
echo $! > my-api.pid
```

Stop:

```bash
kill $(cat my-api.pid)
```

This is fragile — the process won't restart on crash or reboot. Use systemd or Docker for anything beyond throwaway testing.

---

## Reverse Proxy

In production, put nginx or Caddy in front. The Lisp server handles application logic; the reverse proxy handles TLS, static files, rate limiting, and connection buffering.

### nginx

```nginx
server {
    listen 80;
    server_name api.example.com;

    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

### Caddy (auto-TLS)

```
api.example.com {
    reverse_proxy localhost:8000
}
```

Caddy automatically provisions and renews Let's Encrypt certificates.

---

## Agent-Driven Deployment

If an AI agent is deploying and managing the API, Docker Compose is the simplest approach:

```bash
# Agent starts the service
docker compose up -d

# Agent checks health
curl -sf http://localhost:8000/health

# Agent reads logs
docker compose logs --tail=50

# Agent restarts after deploying new code
docker compose down && docker compose up -d --build

# Agent stops the service
docker compose down
```

All commands are non-interactive, return meaningful exit codes, and work identically on Linux and macOS. No service manager configuration needed.

---

## Environment Variables

| Variable     | Default   | Description                      |
|-------------|-----------|----------------------------------|
| `PORT`      | `8000`    | Server listen port               |
| `DB_PATH`   | `app.db`  | Path to SQLite database file     |
| `DEBUG`     | (unset)   | Set to `1` or `true` for debug mode |
| `JWT_SECRET`| (none)    | Secret for JWT signing (if using auth) |

For systemd, put these in `/opt/my-api/.env`. For Docker, use `environment:` in your compose file or a `.env` file. quickapi can generate an `.env.example` via `generate-deployment-files`.

---

## Health Checks

Every quickapi template includes a `/health` endpoint. Use it:

- **systemd**: Add `ExecStartPost` to poll health after start, or use a watchdog.
- **Docker**: The Dockerfile includes `HEALTHCHECK` by default.
- **External monitoring**: Point your uptime monitor at `https://api.example.com/health`.

quickapi can generate a healthcheck script:

```lisp
(quickapi:generate-healthcheck :base-url "http://localhost:8000")
```

---

## Graceful Shutdown

The template's `main` function handles `SIGINT` (Ctrl-C) by calling `(quickapi:stop)`, which tells Clack to stop accepting connections and finish in-flight requests.

- **systemd** sends `SIGTERM` by default, which SBCL treats the same as `SIGINT`.
- **Docker** sends `SIGTERM` on `docker stop`, with a 10-second grace period before `SIGKILL`.
- For custom shutdown logic, extend the `handler-case` in your `main` function.
