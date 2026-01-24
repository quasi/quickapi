# Deployment Guide

Deploy your quickapi application to production.

**Prerequisites**: A working API (complete the [Tutorial](02-tutorial.md) first)

## Overview

This guide covers:

1. Building a standalone executable
2. Docker deployment
3. systemd service
4. Nginx reverse proxy

## Building a Standalone Executable

### With SBCL

Create a build script `build.lisp`:

```lisp
(ql:quickload :quickapi)
(load "todo-api.lisp")

(sb-ext:save-lisp-and-die "todo-api"
  :toplevel #'todo-api:main
  :executable t
  :compression t)
```

Build:

```bash
sbcl --load build.lisp
```

This creates a `todo-api` executable (~30-50MB compressed).

Run:

```bash
./todo-api
```

### Build Script Example

Create `Makefile`:

```makefile
.PHONY: build clean run

build:
	sbcl --load build.lisp

run:
	./todo-api

clean:
	rm -f todo-api todos.db
```

## Docker Deployment

### Dockerfile

```dockerfile
FROM clfoundation/sbcl:latest

# Install system dependencies
RUN apt-get update && apt-get install -y \
    libsqlite3-dev \
    && rm -rf /var/lib/apt/lists/*

# Set up Quicklisp
RUN sbcl --non-interactive \
    --eval '(ql:quickload :quickapi)' \
    --eval '(quit)'

# Copy application
WORKDIR /app
COPY . .

# Build executable
RUN sbcl --non-interactive \
    --load build.lisp

# Expose port
EXPOSE 8000

# Run
CMD ["./todo-api"]
```

### docker-compose.yml

```yaml
version: '3.8'

services:
  api:
    build: .
    ports:
      - "8000:8000"
    volumes:
      - ./data:/app/data
    environment:
      - DB_PATH=/app/data/todos.db
    restart: unless-stopped
```

Build and run:

```bash
docker-compose up -d
```

### Using Environment Variables

Modify your app to read configuration from environment:

```lisp
(defvar *db-path*
  (or (uiop:getenv "DB_PATH") "todos.db"))

(defvar *port*
  (parse-integer (or (uiop:getenv "PORT") "8000")))

(defun main ()
  (init-database)
  (start :port *port*))
```

## systemd Service

Create `/etc/systemd/system/todo-api.service`:

```ini
[Unit]
Description=Todo API
After=network.target

[Service]
Type=simple
User=www-data
Group=www-data
WorkingDirectory=/opt/todo-api
ExecStart=/opt/todo-api/todo-api
Restart=always
RestartSec=5

# Environment
Environment=PORT=8000
Environment=DB_PATH=/opt/todo-api/data/todos.db

# Security hardening
NoNewPrivileges=true
ProtectSystem=strict
ProtectHome=true
ReadWritePaths=/opt/todo-api/data

[Install]
WantedBy=multi-user.target
```

Deploy:

```bash
# Copy executable
sudo mkdir -p /opt/todo-api/data
sudo cp todo-api /opt/todo-api/
sudo chown -R www-data:www-data /opt/todo-api

# Enable and start
sudo systemctl daemon-reload
sudo systemctl enable todo-api
sudo systemctl start todo-api

# Check status
sudo systemctl status todo-api
```

View logs:

```bash
sudo journalctl -u todo-api -f
```

## Nginx Reverse Proxy

### Basic Configuration

Create `/etc/nginx/sites-available/todo-api`:

```nginx
server {
    listen 80;
    server_name api.example.com;

    location / {
        proxy_pass http://127.0.0.1:8000;
        proxy_http_version 1.1;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
    }
}
```

Enable:

```bash
sudo ln -s /etc/nginx/sites-available/todo-api /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

### With SSL (Let's Encrypt)

```bash
sudo apt install certbot python3-certbot-nginx
sudo certbot --nginx -d api.example.com
```

### Rate Limiting

Add to nginx config:

```nginx
# At http level
limit_req_zone $binary_remote_addr zone=api:10m rate=10r/s;

# In location block
location / {
    limit_req zone=api burst=20 nodelay;
    proxy_pass http://127.0.0.1:8000;
    # ... rest of config
}
```

## Health Checks

Add a health endpoint to your API:

```lisp
(api-get "/health" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "status" h) "ok")
    (setf (gethash "timestamp" h) (make-timestamp))
    h))
```

Use in Docker:

```dockerfile
HEALTHCHECK --interval=30s --timeout=3s \
  CMD curl -f http://localhost:8000/health || exit 1
```

## Logging

For production, add request logging. Create a simple logger:

```lisp
(defun log-request (method path status duration)
  (format t "~&~a ~a ~a ~ams~%"
          method path status duration))
```

Redirect output to file in systemd:

```ini
StandardOutput=append:/var/log/todo-api/access.log
StandardError=append:/var/log/todo-api/error.log
```

## Production Checklist

Before deploying:

- [ ] Database file is in persistent location
- [ ] Environment variables configured
- [ ] Health endpoint added
- [ ] Logging configured
- [ ] Error handling covers all routes
- [ ] Validation on all inputs
- [ ] HTTPS configured
- [ ] Rate limiting enabled
- [ ] Backup strategy for database

## Troubleshooting

**Problem**: Connection refused after deploy

**Check**:
1. Is the service running? `systemctl status todo-api`
2. Is it listening? `ss -tlnp | grep 8000`
3. Firewall blocking? `ufw status`

---

**Problem**: Database permission denied

**Fix**: Ensure the user running the service owns the data directory:
```bash
sudo chown -R www-data:www-data /opt/todo-api/data
```

---

**Problem**: Nginx 502 Bad Gateway

**Cause**: Backend not responding.

**Check**: Is the API running and healthy?
```bash
curl http://localhost:8000/health
```

## Next Steps

Your API is deployed. Consider:

- Setting up monitoring (Prometheus, Grafana)
- Database backups (cron + sqlite3 .backup)
- Log aggregation (journald, ELK stack)
- CI/CD pipeline for automated deploys
