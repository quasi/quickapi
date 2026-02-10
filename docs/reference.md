# API Reference

Complete reference for all exported symbols in the `quickapi` package.

## API Definition

### `defapi` (macro)

```lisp
(defapi name &key name version description middlewares)
```

Define an API with metadata and middleware configuration.

**MIDDLEWARES** is a list of middleware specs:
- `:accesslog` — Request logging
- `:session` — Session management
- `:backtrace` — Error backtraces in development
- `(:cors :origins '("*"))` — CORS with options

### `start` (function)

```lisp
(start &key (port 8000) (address "0.0.0.0") (server :hunchentoot) (debug :env))
```

Start the API server. **SERVER** can be `:hunchentoot` (default) or `:woo`. **DEBUG** can be `t`, `nil`, or `:env` (reads `DEBUG` env var).

### `stop` (function)

```lisp
(stop)
```

Stop the running API server.

---

## Routes

All route macros auto-extract path parameters (e.g., `:id` in `/users/:id` binds `id`). Response bodies are automatically JSON-encoded.

**Options in lambda-list:**
- `:auth :jwt` — Require JWT authentication
- `:auth :session` — Require session authentication
- `:auth :api-key` — Require API key authentication

### `api-get` (macro)

```lisp
(api-get uri (&rest options) &body body)
```

Define a GET route.

### `api-post` (macro)

```lisp
(api-post uri (&rest options) &body body)
```

Define a POST route. `*body*` is bound to the parsed JSON request body.

### `api-put` (macro)

```lisp
(api-put uri (&rest options) &body body)
```

Define a PUT route. `*body*` is bound to the parsed JSON request body.

### `api-patch` (macro)

```lisp
(api-patch uri (&rest options) &body body)
```

Define a PATCH route. `*body*` is bound to the parsed JSON request body.

### `api-delete` (macro)

```lisp
(api-delete uri (&rest options) &body body)
```

Define a DELETE route.

---

## Models

### `defmodel` (macro)

```lisp
(defmodel name fields)
```

Define a database model with auto-generated CRUD operations.

**Field options:** `:type`, `:required`, `:default`, `:unique`, `:max-length`, `:min-length`, `:pattern`, `:references`, `:on-delete`

**Supported types:** `string`, `integer`, `boolean`, `float`, `datetime`, `json`

**Generates:** `create-<name>`, `find-<name>`, `find-<name>-by`, `list-<name>s`, `update-<name>`, `delete-<name>`, `count-<name>s`

### `migrate-models` (function)

```lisp
(migrate-models)
```

Create or update tables for all registered models. Safe to call multiple times (uses `CREATE TABLE IF NOT EXISTS`). Must be called within a `with-db` block.

---

## Responses

### `ok` (function)

```lisp
(ok data)
(ok :key1 val1 :key2 val2)
```

Return 200 OK with JSON data. Accepts a hash-table or keyword pairs.

### `created` (function)

```lisp
(created data)
(created :key1 val1 :key2 val2)
```

Return 201 Created with JSON data.

### `no-content` (function)

```lisp
(no-content)
```

Return 204 No Content with empty body.

### `bad-request` (function)

```lisp
(bad-request message &optional details)
```

Signal a 400 Bad Request error.

### `not-found` (function)

```lisp
(not-found &optional message details)
```

Signal a 404 Not Found error. Message defaults to "Resource not found".

### `unauthorized` (function)

```lisp
(unauthorized message)
```

Signal a 401 Unauthorized error.

### `forbidden` (function)

```lisp
(forbidden message)
```

Signal a 403 Forbidden error.

### `conflict` (function)

```lisp
(conflict message &optional details)
```

Signal a 409 Conflict error.

### `internal-error` (function)

```lisp
(internal-error message &optional cause)
```

Signal a 500 Internal Server Error.

### `error-response` (function)

```lisp
(error-response status message &optional details)
```

Signal an HTTP error with a custom status code.

### `json-response` (function)

```lisp
(json-response data &key (status 200))
```

Create a JSON response with the given data and HTTP status code.

---

## Validation

### `validate` (macro)

```lisp
(validate data &body checks)
```

Validate DATA using the provided checks. If any errors are found, signals a `validation-error` condition (returns 422). Collects all errors before signaling.

### `require-fields` (function)

```lisp
(require-fields data &rest fields)
```

Check that all FIELDS are present and non-empty in DATA hash-table.

### `require-type` (function)

```lisp
(require-type data field expected-type)
```

Check that FIELD has the expected type. Supported: `string`, `number`, `integer`, `boolean`, `list`, `hash-table`.

### `require-length` (function)

```lisp
(require-length data field &key min max)
```

Check that the length of FIELD is within bounds. Works for strings, lists, and vectors.

### `require-range` (function)

```lisp
(require-range data field &key min max)
```

Check that numeric value of FIELD is within bounds.

### `require-pattern` (function)

```lisp
(require-pattern data field pattern)
```

Check that FIELD matches the regex PATTERN.

---

## Database

### `with-db` (macro)

```lisp
(with-db (path) &body body)
```

Execute BODY with `*db*` bound to an open database connection. PATH is the database file path, or `":memory:"` for in-memory.

### `with-transaction` (macro)

```lisp
(with-transaction &body body)
```

Execute BODY within a database transaction. Commits on success, rolls back on error.

### `ensure-table` (function)

```lisp
(ensure-table name columns)
```

Create table if it doesn't exist. NAME is a keyword or string. COLUMNS is a list of column definitions.

### `last-insert-id` (function)

```lisp
(last-insert-id)
```

Get the rowid of the last inserted row. Must be called within a `with-db` block.

### `row-to-hash` (function)

```lisp
(row-to-hash row column-names)
```

Convert a single database row to a hash-table.

### `rows-to-json` (function)

```lisp
(rows-to-json rows &optional column-names)
```

Convert database rows to a list of hash-tables suitable for JSON.

---

## Authentication

### `hash-password` (function)

```lisp
(hash-password password)
```

Hash a password using PBKDF2-SHA256 (100,000 iterations). Returns format: `pbkdf2:sha256:iterations:salt-base64:hash-base64`.

### `verify-password` (function)

```lisp
(verify-password password stored-hash)
```

Verify a password against a stored hash. Returns `t` if match, `nil` otherwise. Uses constant-time comparison.

### `generate-jwt` (function)

```lisp
(generate-jwt claims &key (expires-in 3600))
```

Generate a JWT token from CLAIMS (hash-table or plist). EXPIRES-IN is seconds until expiration (default 1 hour).

### `verify-jwt` (function)

```lisp
(verify-jwt token)
```

Verify and decode a JWT token. Returns claims hash-table if valid, `nil` otherwise. Checks signature and expiration.

### `session-get` (function, setf-able)

```lisp
(session-get key)
(setf (session-get key) value)
```

Get or set a value in the current session. Requires `:session` middleware.

---

## Configuration

### `load-env-file` (function)

```lisp
(load-env-file &optional (path ".env"))
```

Load environment variables from a `.env` file. Returns `t` if loaded, `nil` if not found.

### `getenv` (function)

```lisp
(getenv name &key default)
```

Get an environment variable. Checks `.env` values first, then system environment.

### `getenv-int` (function)

```lisp
(getenv-int name &key default)
```

Get an environment variable as an integer.

### `getenv-bool` (function)

```lisp
(getenv-bool name &key default)
```

Get an environment variable as a boolean. Recognizes: `true`, `1`, `yes`, `on`.

### `getenv-list` (function)

```lisp
(getenv-list name &key (separator ",") default)
```

Get an environment variable as a list of strings.

### `require-env` (function)

```lisp
(require-env name)
```

Get an environment variable, signaling an error if not found.

### `ensure-env-loaded` (function)

```lisp
(ensure-env-loaded)
```

Ensure `.env` file is loaded. Call at application startup.

### `reload-env` (function)

```lisp
(reload-env)
```

Reload `.env` file, clearing cached values.

### `print-env-template` (function)

```lisp
(print-env-template routes &key (stream *standard-output*))
```

Print a `.env.example` template with common API settings.

---

## Deployment

### `generate-deployment-files` (function)

```lisp
(generate-deployment-files &key directory system-name api-name port)
```

Generate all deployment files: `bin/start`, `bin/healthcheck`, `scripts/smoke-tests.sh`, `scripts/app.service`, `.env.example`.

### `generate-systemd-unit` (function)

```lisp
(generate-systemd-unit &key stream api-name description user working-dir port)
```

Generate a systemd service unit file.

### `generate-start-script` (function)

```lisp
(generate-start-script &key stream system-name entry-point)
```

Generate a bash start script using sbcl with quicklisp.

### `generate-healthcheck` (function)

```lisp
(generate-healthcheck &key stream base-url endpoint)
```

Generate a healthcheck script for monitoring.

### `generate-smoke-tests` (function)

```lisp
(generate-smoke-tests &key stream base-url api-name)
```

Generate a bash smoke test script with curl commands for each registered route.

---

## Special Variables

| Variable | Description |
|----------|-------------|
| `*body*` | Parsed JSON body of the current request (hash-table or nil) |
| `*current-user*` | Authenticated user data for the current request |
| `*db*` | Current database connection (bind with `with-db`) |
| `*request*` | Current Lack request environment |
| `*debug-mode*` | Whether running in debug mode |
| `*jwt-secret*` | Secret key for JWT signing (set before using JWT auth) |
| `*jwt-algorithm*` | Algorithm for JWT signing (default `:hs256`) |
| `*session-user-loader*` | Function to load user from session user-id |
| `*api-key-validator*` | Function to validate an API key |
| `*models*` | Registry of defined models |

---

## Condition Hierarchy

```
quickapi-error (base)
  ├── http-error
  │   ├── client-error (4xx)
  │   │   ├── bad-request-error (400)
  │   │   ├── unauthorized-error (401)
  │   │   ├── forbidden-error (403)
  │   │   ├── not-found-error (404)
  │   │   ├── conflict-error (409)
  │   │   └── validation-error (422)
  │   └── server-error (5xx)
  │       └── internal-server-error (500)
  ├── database-error
  │   ├── record-not-found (also inherits not-found-error)
  │   ├── duplicate-record (also inherits conflict-error)
  │   └── connection-error
  └── authentication-error (inherits unauthorized-error)
      └── invalid-credentials
```

### Condition Accessors

| Accessor | Condition | Description |
|----------|-----------|-------------|
| `error-message` | `quickapi-error` | Human-readable error message |
| `http-error-status` | `http-error` | HTTP status code |
| `http-error-details` | `http-error` | Additional structured details |
| `validation-errors` | `validation-error` | List of field-specific errors |
| `db-error-operation` | `database-error` | Operation being performed |
| `db-error-table` | `database-error` | Table name |
| `duplicate-field` | `duplicate-record` | Field that caused violation |
| `error-cause` | `internal-server-error` | Underlying error |
