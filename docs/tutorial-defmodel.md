# Tutorial: Build a Todo API with defmodel

Learn to build a complete authenticated todo API using quickapi's `defmodel` macro. By the end, you'll have a working API with user registration, login, and per-user todos.

**Time**: ~20 minutes
**Prerequisites**: [README Quickstart](../README.md#quickstart) completed
**What you'll build**: Multi-user todo API with JWT authentication

## What you'll learn

1. Using `defmodel` to generate database tables and CRUD functions
2. Protecting routes with JWT authentication
3. Building registration and login flows
4. Associating data with authenticated users

## Step 1: Project setup

Create a new file `todo-api.lisp`:

```lisp
(defpackage :todo-api
  (:use :cl :quickapi))

(in-package :todo-api)

;; Define the API with logging middleware
(defapi todo-api
  :version "1.0"
  :middlewares (:accesslog))
```

Load it in your REPL:

```lisp
(load "todo-api.lisp")
```

You should see:
```
;; Loading "todo-api.lisp"
```

✓ Checkpoint: Package loaded successfully.

## Step 2: Define your data models

Add two models - one for users, one for todos:

```lisp
;; User model - stores email and hashed password
(defmodel user
  ((email :type string :required t :unique t)
   (password-hash :type string :required t)))

;; Todo model - belongs to a user
(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :required t)))
```

What just happened:
- `defmodel` generated 7 functions for each model: `create-*`, `find-*`, `find-*-by`, `list-*s`, `update-*`, `delete-*`, `count-*s`
- Each model automatically gets `id`, `created_at`, and `updated_at` columns
- Validation rules (`:required`, `:max-length`) will be enforced on create/update

✓ Checkpoint: Models defined. Functions like `create-user` and `list-todos` now exist.

## Step 3: Create the database tables

Add a function to initialize the database:

```lisp
(defun init-db ()
  "Create database tables from models."
  (with-db ("todos.db")
    (migrate-models)))
```

Run it:

```lisp
(todo-api:init-db)
```

You should see a new file `todos.db` in your directory.

✓ Checkpoint: Database file created with `users` and `todos` tables.

## Step 4: Configure authentication

Add JWT configuration at the top of your file (after `defapi`):

```lisp
;; JWT secret for signing tokens (use a real secret in production)
(setf *jwt-secret* "my-super-secret-jwt-key-change-me-in-production")
```

✓ Checkpoint: JWT auth configured.

## Step 5: User registration

Add a registration endpoint:

```lisp
(api-post "/register" ()
  (with-db ("todos.db")
    ;; Validate input
    (validate *body*
      (require-fields "email" "password")
      (require-type "email" 'string)
      (require-length "password" :min 8)
      (require-pattern "email" "^[^@]+@[^@]+\\.[^@]+$"))

    ;; Hash password before storing
    (let ((user-data (make-hash-table :test 'equal)))
      (setf (gethash "email" user-data) (gethash "email" *body*))
      (setf (gethash "password_hash" user-data)
            (hash-password (gethash "password" *body*)))

      ;; Create user and return (without password hash)
      (let ((user (create-user user-data)))
        (remhash "password_hash" user)
        (created user)))))
```

Reload the file and test:

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"alice@example.com","password":"secret123"}' \
     http://localhost:8000/register
```

Expected response:
```json
{"id":1,"email":"alice@example.com","created_at":"2026-01-25T12:00:00","updated_at":"2026-01-25T12:00:00"}
```

Test validation (password too short):

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"bob@example.com","password":"short"}' \
     http://localhost:8000/register
```

Expected response (422 error):
```json
{"error":"validation_error","message":"Validation failed","details":[{"field":"password","message":"must be at least 8 characters"}]}
```

✓ Checkpoint: User registration working with password hashing and validation.

## Step 6: User login

Add a login endpoint that returns a JWT token:

```lisp
(api-post "/login" ()
  (with-db ("todos.db")
    ;; Find user by email
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          ;; Valid login - return JWT token
          (ok (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "token" h)
                      (generate-jwt
                       (list :sub (gethash "id" user)
                             :email (gethash "email" user))))
                h))
          ;; Invalid credentials
          (error-response 401 "Invalid email or password")))))
```

Test login:

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"alice@example.com","password":"secret123"}' \
     http://localhost:8000/login
```

Expected response:
```json
{"token":"eyJhbGc..."}
```

Copy the token value - you'll need it for protected routes.

Test wrong password:

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"alice@example.com","password":"wrongpass"}' \
     http://localhost:8000/login
```

Expected response (401 error):
```json
{"error":"unauthorized","message":"Invalid email or password"}
```

✓ Checkpoint: Login working, returns JWT tokens.

## Step 7: Create todo (protected route)

Add a protected endpoint to create todos. Note the `:auth :jwt` option:

```lisp
(api-post "/todos" (:auth :jwt)
  (with-db ("todos.db")
    ;; Validate input
    (validate *body*
      (require-fields "title")
      (require-length "title" :min 1 :max 200))

    ;; Get user ID from JWT claims
    (let ((user-id (gethash "sub" *current-user*))
          (todo-data (make-hash-table :test 'equal)))
      (setf (gethash "title" todo-data) (gethash "title" *body*))
      (setf (gethash "user_id" todo-data) user-id)
      (when (gethash "completed" *body*)
        (setf (gethash "completed" todo-data) (gethash "completed" *body*)))

      ;; Create and return todo
      (created (create-todo todo-data)))))
```

Test without token (should fail):

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"title":"Buy milk"}' \
     http://localhost:8000/todos
```

Expected response (401 error):
```json
{"error":"unauthorized","message":"Invalid or expired token"}
```

Test with token (replace YOUR_TOKEN with the token from step 6):

```bash
curl -X POST -H "Content-Type: application/json" \
     -H "Authorization: Bearer YOUR_TOKEN" \
     -d '{"title":"Buy milk"}' \
     http://localhost:8000/todos
```

Expected response:
```json
{"id":1,"title":"Buy milk","completed":false,"user_id":1,"created_at":"2026-01-25T12:05:00","updated_at":"2026-01-25T12:05:00"}
```

✓ Checkpoint: Creating todos works, requires authentication, associates with logged-in user.

## Step 8: List user's todos (protected route)

Add an endpoint to list only the authenticated user's todos:

```lisp
(api-get "/todos" (:auth :jwt)
  (with-db ("todos.db")
    ;; Get user ID from JWT and list only their todos
    (let ((user-id (gethash "sub" *current-user*)))
      (ok (list-todos :where `(:= :user-id ,user-id)
                      :order-by '(:created_at :desc))))))
```

Test:

```bash
curl -H "Authorization: Bearer YOUR_TOKEN" \
     http://localhost:8000/todos
```

Expected response:
```json
[{"id":1,"title":"Buy milk","completed":false,"user_id":1,"created_at":"2026-01-25T12:05:00","updated_at":"2026-01-25T12:05:00"}]
```

✓ Checkpoint: Listing todos shows only the authenticated user's todos.

## Step 9: Update todo (protected route)

Add an endpoint to toggle completion:

```lisp
(api-put "/todos/:id" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*))
          (todo (find-todo id)))

      ;; Check todo exists
      (unless todo
        (not-found "Todo not found"))

      ;; Check user owns this todo
      (unless (= (gethash "user_id" todo) user-id)
        (error-response 403 "Not authorized"))

      ;; Update completed status
      (let ((update-data (make-hash-table :test 'equal)))
        (when (gethash "completed" *body*)
          (setf (gethash "completed" update-data) (gethash "completed" *body*)))
        (when (gethash "title" *body*)
          (setf (gethash "title" update-data) (gethash "title" *body*)))

        (ok (update-todo id update-data))))))
```

Test marking complete:

```bash
curl -X PUT -H "Content-Type: application/json" \
     -H "Authorization: Bearer YOUR_TOKEN" \
     -d '{"completed":true}' \
     http://localhost:8000/todos/1
```

Expected response:
```json
{"id":1,"title":"Buy milk","completed":true,"user_id":1,"created_at":"2026-01-25T12:05:00","updated_at":"2026-01-25T12:10:00"}
```

✓ Checkpoint: Updating todos works, enforces ownership.

## Step 10: Delete todo (protected route)

Add an endpoint to delete todos:

```lisp
(api-delete "/todos/:id" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*))
          (todo (find-todo id)))

      ;; Check todo exists
      (unless todo
        (not-found "Todo not found"))

      ;; Check user owns this todo
      (unless (= (gethash "user_id" todo) user-id)
        (error-response 403 "Not authorized"))

      ;; Delete and return 204
      (delete-todo id)
      (no-content))))
```

Test:

```bash
curl -X DELETE -H "Authorization: Bearer YOUR_TOKEN" \
     http://localhost:8000/todos/1
```

Expected: Empty response with status 204.

Verify deletion:

```bash
curl -H "Authorization: Bearer YOUR_TOKEN" \
     http://localhost:8000/todos
```

Expected response:
```json
[]
```

✓ Checkpoint: Deleting todos works, enforces ownership.

## Step 11: Add a startup function

Add a convenient startup function at the end of the file:

```lisp
(defun main ()
  "Initialize database and start the server."
  (init-db)
  (format t "~&Starting todo API on port 8000...~%")
  (start :port 8000))
```

Now you can start everything with:

```lisp
(todo-api:main)
```

✓ Checkpoint: Server starts with one command.

## Complete workflow test

Let's test the complete workflow:

```bash
# 1. Register a user
curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"test@example.com","password":"password123"}' \
     http://localhost:8000/register

# 2. Login and get token
TOKEN=$(curl -X POST -H "Content-Type: application/json" \
     -d '{"email":"test@example.com","password":"password123"}' \
     http://localhost:8000/login | jq -r '.token')

# 3. Create a todo
curl -X POST -H "Content-Type: application/json" \
     -H "Authorization: Bearer $TOKEN" \
     -d '{"title":"Learn quickapi"}' \
     http://localhost:8000/todos

# 4. List todos
curl -H "Authorization: Bearer $TOKEN" \
     http://localhost:8000/todos

# 5. Mark complete
curl -X PUT -H "Content-Type: application/json" \
     -H "Authorization: Bearer $TOKEN" \
     -d '{"completed":true}' \
     http://localhost:8000/todos/1

# 6. Delete
curl -X DELETE -H "Authorization: Bearer $TOKEN" \
     http://localhost:8000/todos/1
```

## What you learned

- **defmodel**: Generates tables and CRUD functions from simple declarations
- **Authentication**: Adding `:auth :jwt` to routes protects them
- **Password security**: `hash-password` and `verify-password` handle encryption
- **JWT tokens**: `generate-jwt` creates tokens, `*current-user*` accesses claims
- **Validation**: `validate` with `require-*` functions ensures data quality
- **Database queries**: Using `:where` clauses to filter by user
- **Authorization**: Checking ownership before allowing updates/deletes

## Next steps

- **[Authentication Guide](authentication.md)** - Learn about session and API key auth
- **[Middleware Guide](middleware.md)** - Add CORS, rate limiting, etc.
- **[API Reference](reference.md)** - Complete function documentation

## Complete code

The complete `todo-api.lisp` from this tutorial:

```lisp
(defpackage :todo-api
  (:use :cl :quickapi))

(in-package :todo-api)

(defapi todo-api
  :version "1.0"
  :middlewares (:accesslog))

(setf *jwt-secret* "my-super-secret-jwt-key-change-me-in-production")

;; Models
(defmodel user
  ((email :type string :required t :unique t)
   (password-hash :type string :required t)))

(defmodel todo
  ((title :type string :required t :max-length 200)
   (completed :type boolean :default nil)
   (user-id :type integer :required t)))

(defun init-db ()
  (with-db ("todos.db")
    (migrate-models)))

;; Public routes
(api-post "/register" ()
  (with-db ("todos.db")
    (validate *body*
      (require-fields "email" "password")
      (require-type "email" 'string)
      (require-length "password" :min 8)
      (require-pattern "email" "^[^@]+@[^@]+\\.[^@]+$"))
    (let ((user-data (make-hash-table :test 'equal)))
      (setf (gethash "email" user-data) (gethash "email" *body*))
      (setf (gethash "password_hash" user-data)
            (hash-password (gethash "password" *body*)))
      (let ((user (create-user user-data)))
        (remhash "password_hash" user)
        (created user)))))

(api-post "/login" ()
  (with-db ("todos.db")
    (let ((user (find-user-by :email (gethash "email" *body*))))
      (if (and user
               (verify-password (gethash "password" *body*)
                               (gethash "password_hash" user)))
          (ok (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "token" h)
                      (generate-jwt
                       (list :sub (gethash "id" user)
                             :email (gethash "email" user))))
                h))
          (error-response 401 "Invalid email or password")))))

;; Protected routes
(api-post "/todos" (:auth :jwt)
  (with-db ("todos.db")
    (validate *body*
      (require-fields "title")
      (require-length "title" :min 1 :max 200))
    (let ((user-id (gethash "sub" *current-user*))
          (todo-data (make-hash-table :test 'equal)))
      (setf (gethash "title" todo-data) (gethash "title" *body*))
      (setf (gethash "user_id" todo-data) user-id)
      (when (gethash "completed" *body*)
        (setf (gethash "completed" todo-data) (gethash "completed" *body*)))
      (created (create-todo todo-data)))))

(api-get "/todos" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*)))
      (ok (list-todos :where `(:= :user-id ,user-id)
                      :order-by '(:created_at :desc))))))

(api-put "/todos/:id" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*))
          (todo (find-todo id)))
      (unless todo
        (not-found "Todo not found"))
      (unless (= (gethash "user_id" todo) user-id)
        (error-response 403 "Not authorized"))
      (let ((update-data (make-hash-table :test 'equal)))
        (when (gethash "completed" *body*)
          (setf (gethash "completed" update-data) (gethash "completed" *body*)))
        (when (gethash "title" *body*)
          (setf (gethash "title" update-data) (gethash "title" *body*)))
        (ok (update-todo id update-data))))))

(api-delete "/todos/:id" (:auth :jwt)
  (with-db ("todos.db")
    (let ((user-id (gethash "sub" *current-user*))
          (todo (find-todo id)))
      (unless todo
        (not-found "Todo not found"))
      (unless (= (gethash "user_id" todo) user-id)
        (error-response 403 "Not authorized"))
      (delete-todo id)
      (no-content))))

(defun main ()
  (init-db)
  (format t "~&Starting todo API on port 8000...~%")
  (start :port 8000))
```
