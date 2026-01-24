# Authentication Guide

QuickAPI supports three authentication methods out of the box. This guide explains when to use each and how to implement them.

## Which auth method should I use?

| Method | Best For | Pros | Cons |
|--------|----------|------|------|
| **JWT** | Mobile apps, SPAs, microservices | Stateless, scales well, works across domains | Can't revoke tokens early |
| **Sessions** | Traditional web apps | Can revoke anytime, familiar pattern | Requires session storage, doesn't scale as well |
| **API Keys** | Server-to-server, CLI tools, webhooks | Simple, long-lived | Less secure for user auth |

**Quick recommendation**: Use JWT for mobile/SPA, sessions for server-rendered HTML, API keys for service-to-service.

## JWT Authentication

JWT (JSON Web Tokens) are self-contained tokens that carry user information. The server doesn't need to store session state.

### Setup

Configure your JWT secret before using JWT auth:

```lisp
;; Set this once at startup (use a real secret in production)
(setf *jwt-secret* "your-secret-key-must-be-at-least-32-characters-long")
```

**Security note**: Keep this secret safe. Anyone with the secret can forge tokens.

### Login flow

Create a login endpoint that generates and returns a JWT:

```lisp
(api-post "/login" ()
  (with-db ("app.db")
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          ;; Valid login - generate JWT
          (ok (make-hash "token"
                  (generate-jwt
                   (list :sub (gethash "id" user)
                         :email (gethash "email" user)
                         :role (gethash "role" user)))))
          ;; Invalid credentials
          (error-response 401 "Invalid credentials")))))
```

The client receives:
```json
{"token":"eyJhbGciOiJIUzI1NiIsInR5cCI6IkpXVCJ9..."}
```

### Protecting routes

Add `:auth :jwt` to any route that requires authentication:

```lisp
(api-get "/profile" (:auth :jwt)
  ;; *current-user* is automatically bound to the JWT claims
  (ok *current-user*))

(api-post "/todos" (:auth :jwt)
  (let ((user-id (gethash "sub" *current-user*)))
    ;; Use user-id from the token
    (with-db ("app.db")
      (created (create-todo (make-hash "title" (gethash "title" *body*)
                                       "user_id" user-id))))))
```

### Making authenticated requests

Clients must send the token in the `Authorization` header:

```bash
curl -H "Authorization: Bearer eyJhbGc..." \
     http://localhost:8000/profile
```

### Token expiration

Tokens expire after 1 hour by default. Customize with `:expires-in`:

```lisp
;; Token expires in 24 hours
(generate-jwt claims :expires-in (* 24 3600))

;; Token expires in 5 minutes
(generate-jwt claims :expires-in 300)
```

Expired tokens return 401:
```json
{"error":"unauthorized","message":"Invalid or expired token"}
```

### JWT claims

`*current-user*` contains all claims as a hash-table:

```lisp
(api-get "/admin" (:auth :jwt)
  (let ((role (gethash "role" *current-user*)))
    (if (string= role "admin")
        (ok (list-all-users))
        (error-response 403 "Admin only"))))
```

### Refresh tokens

QuickAPI doesn't provide refresh tokens built-in. Implement them yourself:

```lisp
;; Store refresh tokens in database
(defmodel refresh-token
  ((user-id :type integer :required t)
   (token :type string :required t :unique t)
   (expires-at :type datetime :required t)))

(api-post "/refresh" ()
  (let* ((refresh-token (gethash "refresh_token" *body*))
         (token-record (find-refresh-token-by :token refresh-token)))
    (if (and token-record
             (< (get-universal-time) (gethash "expires_at" token-record)))
        ;; Valid refresh token - issue new access token
        (ok (make-hash "token"
                (generate-jwt
                 (list :sub (gethash "user_id" token-record)))))
        (error-response 401 "Invalid refresh token"))))
```

## Session Authentication

Sessions store user state server-side. Good for traditional web applications.

### Setup

Enable session middleware in your API definition:

```lisp
(defapi my-api
  :middlewares (:session))  ; Enables session support
```

Configure how sessions load user data:

```lisp
;; Function to load user from user-id stored in session
(setf *session-user-loader* #'find-user)
```

### Login flow

Store the user ID in the session:

```lisp
(api-post "/login" ()
  (with-db ("app.db")
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          (progn
            ;; Store user ID in session
            (setf (session-get :user-id) (gethash "id" user))
            (ok user))
          (error-response 401 "Invalid credentials")))))
```

### Logout flow

Clear the session:

```lisp
(api-post "/logout" (:auth :session)
  (setf (session-get :user-id) nil)
  (no-content))
```

### Protecting routes

Add `:auth :session` to routes that require authentication:

```lisp
(api-get "/dashboard" (:auth :session)
  ;; *current-user* is automatically loaded via *session-user-loader*
  (ok *current-user*))

(api-post "/todos" (:auth :session)
  (let ((user-id (gethash "id" *current-user*)))
    (with-db ("app.db")
      (created (create-todo (make-hash "title" (gethash "title" *body*)
                                       "user_id" user-id))))))
```

### Session helpers

Get and set session values:

```lisp
;; Get a value
(session-get :user-id)  ; => 42

;; Set a value
(setf (session-get :cart-items) '(1 2 3))

;; Works with any value type
(setf (session-get :preferences) (make-hash "theme" "dark"))
```

### Session storage

By default, sessions are stored in memory. For production, use persistent storage:

```lisp
(defapi my-api
  :middlewares ((:session :store (lack.session.store.redis:make-redis-store))))
```

See [Lack session documentation](https://github.com/fukamachi/lack/tree/master/src/middleware/session) for storage options.

## API Key Authentication

API keys are long-lived tokens good for service-to-service auth or CLI tools.

### Setup

Define a function to validate API keys:

```lisp
;; Function that takes an API key and returns user/service info or nil
(setf *api-key-validator*
  (lambda (key)
    (with-db ("app.db")
      (let ((api-key (find-api-key-by :key key)))
        (when (and api-key
                   (gethash "active" api-key))
          ;; Return associated user or service info
          (find-user (gethash "user_id" api-key)))))))
```

### Store API keys

Create a model for API keys:

```lisp
(defmodel api-key
  ((key :type string :required t :unique t)
   (user-id :type integer :required t)
   (name :type string)  ; "Production Server", "Mobile App", etc.
   (active :type boolean :default t)))
```

### Generate API keys

Create an endpoint to generate keys:

```lisp
(api-post "/api-keys" (:auth :jwt)
  (with-db ("app.db")
    (let* ((user-id (gethash "sub" *current-user*))
           (key (ironclad:byte-array-to-hex-string
                 (ironclad:random-data 32)))
           (api-key-data (make-hash "key" key
                                    "user_id" user-id
                                    "name" (gethash "name" *body*))))
      (created (create-api-key api-key-data)))))
```

### Protecting routes

Add `:auth :api-key` to routes:

```lisp
(api-get "/data" (:auth :api-key)
  ;; *current-user* contains the result from *api-key-validator*
  (ok (fetch-data-for-user (gethash "id" *current-user*))))
```

### Making authenticated requests

API keys can be sent two ways:

**1. In X-API-Key header (recommended):**

```bash
curl -H "X-API-Key: your-api-key-here" \
     http://localhost:8000/data
```

**2. In query parameter:**

```bash
curl "http://localhost:8000/data?api_key=your-api-key-here"
```

### Revoking API keys

Mark keys as inactive:

```lisp
(api-delete "/api-keys/:id" (:auth :jwt)
  (with-db ("app.db")
    (let ((user-id (gethash "sub" *current-user*))
          (api-key (find-api-key id)))
      (unless (= (gethash "user_id" api-key) user-id)
        (error-response 403 "Not authorized"))
      ;; Soft delete - mark as inactive
      (update-api-key id (make-hash "active" nil))
      (no-content))))
```

## Password hashing

Always hash passwords before storing. QuickAPI uses PBKDF2-SHA256.

### Hashing passwords

```lisp
(api-post "/register" ()
  (with-db ("app.db")
    (let ((user-data (copy-hash-table *body*)))
      ;; Hash the password
      (setf (gethash "password_hash" user-data)
            (hash-password (gethash "password" user-data)))
      ;; Remove plain password
      (remhash "password" user-data)
      (created (create-user user-data)))))
```

### Verifying passwords

```lisp
(api-post "/login" ()
  (with-db ("app.db")
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               ;; Verify password against hash
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          (ok (make-hash "token" (generate-jwt ...)))
          (error-response 401 "Invalid credentials")))))
```

### How it works

- Uses PBKDF2-SHA256 with 100,000 iterations (OWASP 2023 recommendation)
- Each password gets a unique random salt
- Hash format: `pbkdf2:sha256:100000:salt-base64:hash-base64`
- Verification uses constant-time comparison (prevents timing attacks)

## Combining authentication methods

You can support multiple auth methods in the same API:

```lisp
;; JWT for mobile apps
(api-get "/mobile/profile" (:auth :jwt)
  (ok *current-user*))

;; Session for web app
(api-get "/web/dashboard" (:auth :session)
  (ok *current-user*))

;; API key for server-to-server
(api-get "/api/data" (:auth :api-key)
  (ok *current-user*))
```

## Security best practices

### JWT

- ✓ Use a strong secret (32+ random characters)
- ✓ Set reasonable expiration times (1 hour for access tokens)
- ✓ Use HTTPS in production
- ✓ Don't store sensitive data in JWT claims (they're visible)
- ✗ Don't put the secret in source control

### Sessions

- ✓ Use secure session cookies (`HttpOnly`, `Secure`, `SameSite`)
- ✓ Use persistent session storage in production (Redis, PostgreSQL)
- ✓ Implement session timeouts
- ✓ Regenerate session ID after login
- ✗ Don't store sensitive data client-side

### API Keys

- ✓ Generate keys with cryptographically secure random data
- ✓ Use HTTPS in production (keys sent in plain text)
- ✓ Allow users to revoke keys
- ✓ Log API key usage for audit trails
- ✗ Don't use API keys for user authentication (use JWT or sessions)

### Passwords

- ✓ Require minimum length (8+ characters)
- ✓ Use `hash-password` (PBKDF2-SHA256)
- ✓ Never log or display passwords
- ✗ Don't return password hashes in API responses
- ✗ Don't implement your own hashing (use provided functions)

## Troubleshooting

### Error: "JWT secret not configured"

**Cause**: Trying to use `:auth :jwt` without setting `*jwt-secret*`.

**Fix**: Set the secret before starting the server:

```lisp
(setf *jwt-secret* "your-secret-key-at-least-32-characters")
```

### Error: "Session required"

**Cause**: Using `:auth :session` without session middleware.

**Fix**: Add `:session` to middlewares:

```lisp
(defapi my-api
  :middlewares (:session))
```

### Error: "Invalid or expired token"

**Causes**:
- Token has expired (check `exp` claim)
- Wrong secret used to sign/verify
- Token corrupted or tampered with

**Fix**: Generate a new token by logging in again.

### `*current-user*` is nil in protected routes

**Cause**: Authentication passed but user loader failed.

**Fix for sessions**: Ensure `*session-user-loader*` is set and returns valid data:

```lisp
(setf *session-user-loader*
  (lambda (user-id)
    (with-db ("app.db")
      (find-user user-id))))
```

**Fix for API keys**: Ensure `*api-key-validator*` is set and returns valid data:

```lisp
(setf *api-key-validator*
  (lambda (key)
    (with-db ("app.db")
      (let ((api-key (find-api-key-by :key key)))
        (when api-key
          (find-user (gethash "user_id" api-key)))))))
```

## Next steps

- **[Middleware Guide](middleware.md)** - CORS, rate limiting, logging
- **[Tutorial](tutorial-defmodel.md)** - Build a complete authenticated API
- **[API Reference](reference.md)** - Complete function documentation
