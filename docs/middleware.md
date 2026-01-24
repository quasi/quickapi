# Middleware Guide

Middleware processes requests before they reach your route handlers. QuickAPI uses Lack middleware, giving you access to battle-tested components for CORS, logging, sessions, and more.

## Quick start

Add middleware to your API definition:

```lisp
(defapi my-api
  :version "1.0"
  :middlewares (:accesslog          ; Request logging
                :session             ; Session support
                :backtrace))         ; Error details in development
```

That's it. Middleware runs automatically for all routes.

## Common middleware

### Access logging

Logs every request to stdout. Essential for debugging.

```lisp
(defapi my-api
  :middlewares (:accesslog))
```

Output:
```
127.0.0.1 - [25/Jan/2026:12:00:00 +0000] "GET /todos HTTP/1.1" 200 145 "-" "curl/7.79.1"
```

**When to use**: Always. Enable in development and production.

### CORS (Cross-Origin Resource Sharing)

Allows browsers to make requests from different domains. Required for SPAs hosted separately from your API.

```lisp
(defapi my-api
  :middlewares ((:cors :origins '("*"))))  ; Allow all origins
```

For production, specify allowed origins:

```lisp
(defapi my-api
  :middlewares ((:cors :origins '("https://myapp.com"
                                  "https://app.myapp.com"))))
```

**When to use**: When your frontend is served from a different domain than your API.

**Common error without CORS**:
```
Access to fetch at 'http://localhost:8000/todos' from origin 'http://localhost:3000'
has been blocked by CORS policy: No 'Access-Control-Allow-Origin' header is present.
```

### Sessions

Stores user state server-side. Required for `:auth :session`.

```lisp
(defapi my-api
  :middlewares (:session))
```

Access session data in routes:

```lisp
(api-get "/cart" ()
  (let ((items (session-get :cart-items)))
    (ok items)))

(api-post "/cart/add" ()
  (let ((items (or (session-get :cart-items) '())))
    (setf (session-get :cart-items)
          (cons (gethash "item_id" *body*) items))
    (ok (session-get :cart-items))))
```

**When to use**: For traditional web apps, shopping carts, wizards, any server-side state.

### Backtrace

Shows detailed error messages with stack traces. Useful in development, dangerous in production.

```lisp
(defapi my-api
  :middlewares (:backtrace))  ; Development only!
```

With backtrace enabled, errors show full details:
```json
{
  "error": "internal_error",
  "message": "The value NIL is not of type NUMBER.",
  "backtrace": ["(+ NIL 1)", "..."]
}
```

Without backtrace:
```json
{
  "error": "internal_error",
  "message": "Internal server error"
}
```

**When to use**: Development only. Remove before deploying to production.

## Middleware order matters

Middleware wraps your application in layers. Order determines execution sequence:

```lisp
(defapi my-api
  :middlewares (:accesslog      ; 1. Logs request
                :cors           ; 2. Adds CORS headers
                :session        ; 3. Loads session
                :backtrace))    ; 4. Catches errors
```

Request flows through middleware top-to-bottom, then back up:

```
Request
  ↓
:accesslog (logs incoming request)
  ↓
:cors (adds headers)
  ↓
:session (loads session)
  ↓
:backtrace (error handler)
  ↓
Your route handler
  ↓
:backtrace (catches errors)
  ↓
:session (saves session)
  ↓
:cors (no-op on response)
  ↓
:accesslog (logs response)
  ↓
Response
```

**Rule of thumb**: Put logging first, error handling last.

## Production middleware setup

Recommended middleware for production:

```lisp
(defapi production-api
  :middlewares (:accesslog                          ; Always log requests
                (:cors :origins '("https://app.com"))  ; Specific origins
                :session))                           ; Session support
```

What's missing:
- ✗ `:backtrace` - Don't expose stack traces in production

## Development middleware setup

Recommended middleware for development:

```lisp
(defapi dev-api
  :middlewares (:accesslog                  ; See request logs
                (:cors :origins '("*"))     ; Allow all origins
                :session
                :backtrace))                ; Detailed errors
```

## Environment-based middleware

Switch middleware based on environment:

```lisp
(defun get-middlewares ()
  (if (string= (uiop:getenv "ENV") "production")
      (list :accesslog
            (list :cors :origins '("https://app.com"))
            :session)
      (list :accesslog
            (list :cors :origins '("*"))
            :session
            :backtrace)))

(defapi my-api
  :middlewares (get-middlewares))
```

Run in production:
```bash
ENV=production sbcl --load app.lisp
```

Run in development:
```bash
ENV=development sbcl --load app.lisp
```

## Available middleware

QuickAPI supports these Lack middleware out of the box:

| Middleware | Purpose | Configuration |
|------------|---------|---------------|
| `:accesslog` | Request logging | None |
| `:cors` | Cross-origin requests | `:origins` list |
| `:session` | Session storage | `:store`, `:state` (optional) |
| `:backtrace` | Error details | None |

For other middleware, see the [Lack documentation](https://github.com/fukamachi/lack).

## Configuring CORS in detail

### Allow specific origins

```lisp
(defapi my-api
  :middlewares ((:cors :origins '("https://app.example.com"
                                  "https://admin.example.com"))))
```

### Allow all origins (development only)

```lisp
(defapi my-api
  :middlewares ((:cors :origins '("*"))))
```

### CORS with credentials

If your frontend sends cookies or auth headers:

```lisp
(defapi my-api
  :middlewares ((:cors :origins '("https://app.example.com")
                       :credentials t)))
```

Frontend must also set:
```javascript
fetch('http://localhost:8000/api', {
  credentials: 'include'  // Required
})
```

## Session storage

Default session storage is in-memory. Fine for development, problematic for production.

### Redis sessions (production)

```lisp
;; Load Redis session store
(ql:quickload :lack-session-store-redis)

(defapi my-api
  :middlewares ((:session :store (lack.session.store.redis:make-redis-store
                                  :host "localhost"
                                  :port 6379))))
```

### File-based sessions

```lisp
(ql:quickload :lack-session-store-file)

(defapi my-api
  :middlewares ((:session :store (lack.session.store.file:make-file-store
                                  :directory "/tmp/sessions/"))))
```

### Database sessions

```lisp
(ql:quickload :lack-session-store-dbi)

(defapi my-api
  :middlewares ((:session :store (lack.session.store.dbi:make-dbi-store
                                  :connector (lambda ()
                                              (dbi:connect :sqlite3
                                                          :database-name "sessions.db"))))))
```

## Troubleshooting

### CORS not working

**Symptom**: Browser shows CORS error even with `:cors` middleware.

**Cause**: Origin not in allowed list.

**Fix**: Check browser's actual origin and add it:

```lisp
;; Browser shows origin as http://localhost:3000
(defapi my-api
  :middlewares ((:cors :origins '("http://localhost:3000"))))
```

### Sessions not persisting

**Symptom**: Session data disappears between requests.

**Cause**: Using in-memory sessions with multiple server instances or restarts.

**Fix**: Use persistent session storage (Redis, database, file).

### Middleware not applied

**Symptom**: Middleware doesn't seem to run.

**Cause**: Middleware only applies after server restart.

**Fix**: Restart the server:

```lisp
(stop)
(start :port 8000)
```

### Error: "Unknown middleware"

**Symptom**: Warning about unknown middleware when starting server.

**Cause**: Typo in middleware name or middleware not installed.

**Fix**: Check spelling and ensure middleware system is loaded:

```lisp
;; For Redis sessions
(ql:quickload :lack-middleware-session)
(ql:quickload :lack-session-store-redis)
```

## Writing custom middleware

QuickAPI middleware is just Lack middleware. Create your own:

```lisp
(defun my-custom-middleware (app)
  "Adds a custom header to all responses."
  (lambda (env)
    (let ((response (funcall app env)))
      ;; response is (status headers body)
      (let ((status (first response))
            (headers (second response))
            (body (third response)))
        ;; Add custom header
        (list status
              (append headers '(:x-custom-header "my-value"))
              body)))))

(defapi my-api
  :middlewares (my-custom-middleware
                :accesslog))
```

For more complex middleware, see [Lack's middleware guide](https://github.com/fukamachi/lack#middleware).

## Next steps

- **[Authentication Guide](authentication.md)** - Protect routes with JWT, sessions, or API keys
- **[Tutorial](tutorial-defmodel.md)** - Build a complete API
- **[Lack Documentation](https://github.com/fukamachi/lack)** - Full middleware reference
