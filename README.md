# quickapi

Build JSON APIs in Common Lisp with minimal boilerplate.

## Why quickapi?

You need an API endpoint fast. Frameworks like Hunchentoot are powerful but heavy. quickapi is a thin, opinionated layer that handles routing, JSON serialization, validation, and SQLite integration—nothing more.

**Perfect for**: REST APIs in small-to-medium Common Lisp projects where you want to focus on business logic, not infrastructure.

## What's included

- **Minimal DSL**: 5 HTTP route macros (`api-get`, `api-post`, `api-put`, `api-delete`, `api-patch`)
- **Automatic JSON**: Response bodies serialize automatically. Request bodies parse automatically.
- **Built-in validation**: Validate required fields, types, lengths, ranges, and patterns
- **SQLite integration**: Database helpers for CRUD operations and transactions
- **Error handling**: Standardized error responses (`not-found`, `bad-request`, etc.)
- **Request context**: Access parsed body, path parameters, and query parameters

No middleware. No configuration files. No heavy dependencies (except Snooze for routing and Hunchentoot for HTTP).

## Quickstart

### Prerequisites

- SBCL or another Common Lisp implementation
- Quicklisp installed
- `cl-sqlite` configured (see [Database Guide](docs/03-database.md) if needed)

### Install

Add quickapi to your ASDF load path:

```bash
cd ~/quicklisp/local-projects
git clone https://github.com/yourusername/quickapi.git quickapi
```

Load in your REPL:

```lisp
(ql:quickload :quickapi)
```

### Your first API (5 minutes)

Create a file `hello.lisp`:

```lisp
(defpackage :hello
  (:use :cl :quickapi))

(in-package :hello)

;; Define your API
(defapi my-api :name "My First API" :version "1.0")

;; Add a route
(api-get "/" ()
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "message" response) "Hello, World!")
    response))

;; Start the server
(start :port 8000)
```

Load and run:

```lisp
(load "hello.lisp")
```

Expected output:

```
Server started on http://0.0.0.0:8000/
```

Verify it works:

```bash
curl http://localhost:8000/
```

Expected response:

```json
{"message":"Hello, World!"}
```

✓ You now have a working JSON API.

## Common patterns

### Path parameters

```lisp
(api-get "/users/:id" (id)
  (find-user id))
```

### Request body (POST/PUT/PATCH)

The request body is automatically parsed as JSON and bound to `*body*`:

```lisp
(api-post "/users" ()
  (validate *body*
    (require-fields "name" "email"))
  (create-user *body*))
```

### Validation

```lisp
(validate *body*
  (require-fields "title" "email")
  (require-type "email" 'string)
  (require-length "title" :min 1 :max 200)
  (require-range "age" :min 0 :max 150)
  (require-pattern "email" ".*@.*\\..*"))  ; Simple email check
```

If validation fails, an automatic 400 response is sent with error details.

### Error responses

```lisp
(api-get "/todos/:id" (id)
  (let ((todo (find-todo id)))
    (if todo
        todo
        (not-found "Todo not found"))))
```

Built-in helpers: `ok`, `created`, `no-content`, `bad-request`, `not-found`, `error-response`.

### Database operations

```lisp
(api-post "/todos" ()
  (with-db ("todos.db")
    (ensure-table :todos
      '((id integer :primary-key :autoincrement)
        (title text :not-null)
        (completed integer :not-null)))

    (sqlite:insert *db* :todos
      (list :title (gethash "title" *body*)
            :completed 0))

    (let ((id (last-insert-id)))
      (created (sqlite:select *db* :todos :where `(:= :id ,id))))))
```

## Next steps

- **[Quickstart guide](docs/01-quickstart.md)** - Step-by-step setup
- **[Tutorial: Build a Todo API](docs/02-tutorial.md)** - Complete CRUD example with database
- **[Database guide](docs/03-database.md)** - SQLite integration and helpers
- **[Deployment](docs/04-deployment.md)** - Production setup

## Complete example

See `examples/todo-api.lisp` for a full CRUD API with:
- All 5 HTTP methods
- SQLite persistence
- Request validation
- Error handling
- Toggle endpoints

Run it:

```lisp
(load "examples/todo-api.lisp")
(todo-api:main)
```

## Troubleshooting

### Error: "Package QUICKAPI not found"

**Cause**: quickapi is not in your ASDF load path.

**Fix**: Clone to `~/quicklisp/local-projects/` and ensure Quicklisp is configured:

```lisp
(ql:where-is-system :quickapi)
```

Should return a path, not NIL.

### Error: "Hunchentoot already running on port 8000"

**Cause**: Server is already listening on that port.

**Fix**: Stop the server and restart:

```lisp
(quickapi:stop)
(quickapi:start :port 8001)  ; Use a different port
```

### Routes not working after reload

**Cause**: Route definitions are cached in Snooze.

**Fix**: Reload the entire package:

```lisp
(asdf:load-system :quickapi :force t)
(load "your-api.lisp")
```

## Status

Early stage. Core API routing and database operations are stable. Validation and error handling are complete. Testing against real-world APIs ongoing.

## License

MIT - see [LICENSE](LICENSE)
