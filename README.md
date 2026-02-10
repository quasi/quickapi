# quickAPI

Build JSON APIs in Common Lisp with zero boilerplate. Simple. Beginner friendly. Deployment template available.

Agent friendly, point your agent to this file and he should be able to use this library just fine.

> This Library is designed by me and implemented by Claude with my inputs. This is designed for consumption by Agents.  If you have a problem with Agent written code then this library is not for you.

## Why quickAPI?

Most web frameworks overwhelm beginners with configuration, middleware chains, and architectural decisions. quickapi gives you three simple tools:

1. **defmodel** - Declare your data, get CRUD functions automatically
2. **api-get/post/put/delete** - Define routes that just work
3. **Authentication helpers** - JWT, sessions, or API keys in one line

Perfect for building REST APIs when you want to focus on your application logic, not infrastructure setup.

## What's included

- **Database models**: `defmodel` generates tables and CRUD functions automatically
- **5 HTTP route macros**: `api-get`, `api-post`, `api-put`, `api-delete`, `api-patch`
- **Automatic JSON**: Responses serialize automatically, requests parse automatically
- **Built-in validation**: Required fields, types, lengths, ranges, patterns
- **Authentication**: JWT, sessions, and API keys built-in
- **Middleware support**: CORS, logging, sessions via Lack
- **SQLite integration**: Zero-config database for prototypes and small apps

No XML config. No class hierarchies. No complex framework.

## Quickstart

### Prerequisites

- SBCL or another Common Lisp implementation
- Quicklisp and a bunch of libraries (Clack/Lack, cl-base64, ironclad, Jose, cl-ppcre, jzon)

### Install

```bash
cd ~/.quicklisp/local-projects
git clone https://github.com/quasi/inquisitio.git
git clone https://github.com/quasi/quickapi.git
```

Load in your REPL:

```lisp
(ql:quickload :quickapi)
```

### Your first API (3 minutes)

Create `hello.lisp`:

```lisp
(defpackage :hello-api
  (:use :cl :quickapi))

(in-package :hello-api)

;; Define your API
(defapi hello-api
  :version "1.0"
  :middlewares (:accesslog))

;; Define a data model (auto-generates table + CRUD functions)
(defmodel user
  ((name :type string :required t)
   (email :type string :required t :unique t)))

;; Define routes
(api-get "/users" ()
  (with-db ("app.db")
    (ok (list-users))))

(api-post "/users" ()
  (with-db ("app.db")
    (created (create-user *body*))))

;; Start the server
(defun start-app ()
  (with-db ("app.db")
    (migrate-models))  ; Create tables
  (start :port 8000))
```

Load and run:

```lisp
(load "hello.lisp")
(hello-api:start-app)
```

Expected output:
```
Server started on http://0.0.0.0:8000/
```

Test it:

```bash
# Create a user
curl -X POST -H "Content-Type: application/json" \
     -d '{"name":"Alice","email":"alice@example.com"}' \
     http://localhost:8000/users

# List users
curl http://localhost:8000/users
```

Expected response:
```json
[{"id":1,"name":"Alice","email":"alice@example.com","created_at":"2026-01-25T12:00:00","updated_at":"2026-01-25T12:00:00"}]
```

✓ You now have a working API with database persistence.

## Documentation

- **[Tutorial: Todo API with defmodel](docs/tutorial-defmodel.md)** - Build a complete API using models
- **[Authentication Guide](docs/authentication.md)** - JWT, sessions, and API keys
- **[Middleware Guide](docs/middleware.md)** - CORS, logging, sessions
- **[API Reference](docs/reference.md)** - Complete function reference
- **[Deployment](docs/deployment.md)** - Strategies


## Core concepts

### defmodel - Declare your data

Instead of writing SQL and CRUD functions manually, declare your model:

```lisp
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :references user)))
```

This automatically generates:
- Database table with `id`, `created_at`, `updated_at` columns
- `create-todo` - Insert a record
- `find-todo` - Find by ID
- `find-todo-by` - Find by any field
- `list-todos` - List with pagination (`:limit`, `:offset`, `:order-by`)
- `update-todo` - Update fields
- `delete-todo` - Delete by ID
- `count-todos` - Count records

Use them immediately:

```lisp
(with-db ("app.db")
  (migrate-models)  ; Create tables from all models

  (create-todo (make-hash "title" "Buy milk"))
  (find-todo 1)
  (list-todos :limit 10 :order-by '(:created_at :desc))
  (update-todo 1 (make-hash "completed" t))
  (delete-todo 1))
```

### Routes - Define your API

Path parameters like `:id` are automatically bound as variables:

```lisp
(api-get "/todos/:id" ()
  (with-db ("app.db")
    (if-let ((todo (find-todo id)))
      (ok todo)
      (not-found))))

(api-delete "/todos/:id" ()
  (with-db ("app.db")
    (delete-todo id)
    (no-content)))
```

The request body is automatically parsed as JSON and available in `*body*`:

```lisp
(api-post "/todos" ()
  (with-db ("app.db")
    (validate *body*
      (require-fields "title"))
    (created (create-todo *body*))))
```

### Authentication - Protect your routes

Add `:auth` to any route:

```lisp
;; Configure JWT secret
(setf *jwt-secret* "your-secret-key-at-least-32-chars")

;; Public login endpoint
(api-post "/login" ()
  (let ((user (find-user-by :email (gethash "email" *body*))))
    (when (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
      (ok (list :token (generate-jwt (list :sub (gethash "id" user))))))))

;; Protected route - requires JWT
(api-get "/profile" (:auth :jwt)
  (ok *current-user*))  ; *current-user* contains JWT claims

;; Protected route - requires session
(api-get "/dashboard" (:auth :session)
  (ok *current-user*))  ; *current-user* loaded from session
```

Three authentication methods available:
- **JWT** - Stateless tokens (`:auth :jwt`)
- **Sessions** - Server-side sessions (`:auth :session`)
- **API Keys** - Header or query param (`:auth :api-key`)

## Common patterns

### Validation

```lisp
(api-post "/users" ()
  (validate *body*
    (require-fields "name" "email" "password")
    (require-type "email" 'string)
    (require-length "password" :min 8)
    (require-pattern "email" "^[^@]+@[^@]+\\.[^@]+$"))
  (with-db ("app.db")
    (created (create-user *body*))))
```

If validation fails, automatic 422 response with error details.

### Password hashing

```lisp
(api-post "/register" ()
  (validate *body*
    (require-fields "email" "password"))

  (with-db ("app.db")
    (let ((user-data (copy-hash-table *body*)))
      ;; Hash password before storing
      (setf (gethash "password_hash" user-data)
            (hash-password (gethash "password" user-data)))
      (remhash "password" user-data)
      (created (create-user user-data)))))
```

`hash-password` uses PBKDF2-SHA256 with 100,000 iterations.

### Middleware

Enable middleware in your API definition:

```lisp
(defapi my-api
  :middlewares (:accesslog                    ; Request logging
                (:cors :origins '("*"))       ; CORS headers
                :session                       ; Session support
                :backtrace))                  ; Error backtraces
```

### Error responses

Built-in helpers for common HTTP responses:

```lisp
(ok data)                    ; 200 with JSON data
(created data)               ; 201 with JSON data
(no-content)                 ; 204 empty response
(bad-request "message")      ; 400 error
(not-found "message")        ; 404 error
(error-response status msg)  ; Custom error
```

## Complete example

Here's a complete authenticated todo API:

```lisp
(defpackage :todo-api
  (:use :cl :quickapi))

(in-package :todo-api)

(defapi todo-api
  :version "1.0"
  :middlewares (:accesslog :session))

;; Models
(defmodel user
  ((email :type string :required t :unique t)
   (password-hash :type string :required t)))

(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :required t)))

;; Configure auth
(setf *jwt-secret* "your-secret-key-min-32-chars")

;; Public routes
(api-post "/register" ()
  (with-db ("todos.db")
    (validate *body*
      (require-fields "email" "password")
      (require-length "password" :min 8))

    (let ((user-data (copy-hash-table *body*)))
      (setf (gethash "password_hash" user-data)
            (hash-password (gethash "password" user-data)))
      (remhash "password" user-data)
      (created (create-user user-data)))))

(api-post "/login" ()
  (with-db ("todos.db")
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          (ok (list :token (generate-jwt
                            (list :sub (gethash "id" user)))))
          (error-response 401 "Invalid credentials")))))

;; Protected routes
(api-get "/todos" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*)))
      (ok (list-todos :where `(:= :user-id ,user-id))))))

(api-post "/todos" (:auth :jwt)
  (with-db ("todos.db")
    (validate *body* (require-fields "title"))
    (let ((todo-data (copy-hash-table *body*)))
      (setf (gethash "user_id" todo-data)
            (gethash "sub" *current-user*))
      (created (create-todo todo-data)))))

(api-delete "/todos/:id" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*)))
      (if-let ((todo (find-todo id)))
        (if (= (gethash "user_id" todo) user-id)
            (progn
              (delete-todo id)
              (no-content))
            (error-response 403 "Not authorized"))
        (not-found)))))

;; Startup
(defun main ()
  (with-db ("todos.db")
    (migrate-models))
  (start :port 8000))
```

Run it:

```lisp
(load "todo-api.lisp")
(todo-api:main)
```

## Troubleshooting

### Error: "Package QUICKAPI not found"

**Cause**: Not in ASDF load path.

**Fix**: Clone to `~/quicklisp/local-projects/` and verify:

```lisp
(ql:where-is-system :quickapi)
```

### Error: "JWT secret not configured"

**Cause**: Trying to use JWT auth without setting secret.

**Fix**: Set the secret before using JWT:

```lisp
(setf quickapi:*jwt-secret* "your-secret-key-at-least-32-chars")
```

### Routes return 404 after code reload

**Cause**: Route definitions need to be re-evaluated.

**Fix**: Reload your API file:

```lisp
(load "your-api.lisp")
```

### Database locked errors

**Cause**: Multiple connections or uncommitted transactions.

**Fix**: Always use `with-db` which handles connections properly:

```lisp
(with-db ("app.db")
  (create-user data))
```

## Project status

QuickAPI:
- ✓ defmodel macro for database models
- ✓ Clack/Lack middleware support
- ✓ Authentication (JWT, sessions, API keys)

**284 tests passing**. Ready for building APIs.

## License

MIT - see [LICENSE](LICENSE)

## Author

quasi / quasiLabs 2026
