# QuickAPI Condition System

QuickAPI uses Common Lisp's powerful condition system for error handling. This provides better error recovery, interactive debugging, and cleaner error handling than traditional exceptions.

## Quick Start

### Signaling Errors

Use specific error functions instead of generic `error`:

```lisp
;; Instead of generic errors
(error "User not found")  ; ❌ Old way

;; Use specific conditions
(not-found "User not found")              ; ✅ Signals not-found-error
(bad-request "Invalid email format")       ; ✅ Signals bad-request-error
(unauthorized "Login required")            ; ✅ Signals unauthorized-error
(conflict "Email already exists")          ; ✅ Signals conflict-error
(internal-error "Database connection lost") ; ✅ Signals internal-server-error
```

### Handling Errors

Catch specific error types:

```lisp
(handler-case
    (find-user 123)
  (record-not-found (e)
    ;; Handle missing record
    (create-default-user))
  (database-error (e)
    ;; Handle any database error
    (log-error e)
    (retry-with-backoff)))
```

### Using Restarts

Restarts let you recover from errors **interactively** or **programmatically**:

```lisp
;; In your code - model-find offers these restarts
(handler-bind ((record-not-found
                 (lambda (c)
                   (invoke-restart 'return-nil))))  ; Use nil instead of erroring
  (let ((user (find-user 999)))
    (or user (make-guest-user))))

;; Or in the REPL during development:
(find-user 999)
; Debugger invoked on RECORD-NOT-FOUND:
;   Record with id 999 not found in table users
;
; Restarts:
;  0: [USE-VALUE] Provide a record (hash-table) to use instead
;  1: [RETURN-NIL] Return NIL instead of signaling an error
;  2: [RETRY] Retry the database lookup
;  3: [ABORT] Return to REPL
```

## Condition Hierarchy

```
quickapi-error (base for all errors)
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
└── authentication-error (also inherits unauthorized-error)
    └── invalid-credentials
```

## Common Patterns

### Pattern 1: Handle specific errors

```lisp
(defun get-user-profile (id)
  (handler-case
      (let ((user (find-user id)))
        (ok :user user))
    (record-not-found ()
      (not-found (format nil "User ~a does not exist" id)))))
```

### Pattern 2: Use restarts for recovery

```lisp
(defun batch-import-users (user-list)
  (handler-bind ((record-not-found #'(lambda (c)
                                       (invoke-restart 'use-value
                                                      (make-default-user)))))
    (mapcar #'process-user user-list)))  ; Missing users get defaults
```

### Pattern 3: Catch error categories

```lisp
(defun api-call-with-retry ()
  (handler-case
      (make-api-call)
    ;; Catch all client errors (4xx)
    (client-error (e)
      (log-client-error e)
      (bad-request (error-message e)))
    ;; Catch all server errors (5xx)
    (server-error (e)
      (alert-ops e)
      (internal-error "Service temporarily unavailable"))))
```

### Pattern 4: Validation with restarts

```lisp
(defun create-user (data)
  (handler-bind ((validation-error
                   (lambda (c)
                     ;; Auto-fill defaults for missing fields
                     (invoke-restart 'use-defaults))))
    (validate data
      (require-fields "name" "email"))
    (insert-user data)))
```

## Available Restarts

### model-find, model-find-by

- `USE-VALUE` - Provide a record to use instead of erroring
- `RETURN-NIL` - Return NIL instead of signaling error
- `RETRY` - Retry the database lookup

### validate

- `SKIP-VALIDATION` - Skip validation and proceed with data as-is
- `USE-DEFAULTS` - Fill missing/invalid fields with defaults

## Accessing Error Details

All conditions provide useful accessors:

```lisp
(handler-case
    (find-user 123)
  (record-not-found (e)
    (format t "Operation: ~a~%" (db-error-operation e))  ; SELECT
    (format t "Table: ~a~%" (db-error-table e))          ; users
    (format t "Message: ~a~%" (error-message e))         ; Record with id...
    (not-found (error-message e))))
```

## Why This Matters

**Traditional exceptions (Java/Python style):**
- Errors propagate up until caught
- Limited recovery options
- Stack unwinding loses context

**Common Lisp conditions:**
- Error handling **separated from** error recovery
- Callers can recover **without unwinding stack**
- Interactive debugging with restarts
- Better composition and reusability

## For Beginners

If you're new to Common Lisp's condition system:

1. **For simple cases**: Just use the helper functions like `not-found`, `bad-request`
2. **For error handling**: Use `handler-case` like you would try/catch
3. **For advanced recovery**: Learn about `handler-bind` and `invoke-restart`

See [CLtL2 Chapter on Conditions](http://www.cs.cmu.edu/Groups/AI/html/cltl/clm/node312.html) for more details.
