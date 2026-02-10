# Tutorial: Build a Todo API (Low-Level Approach)

> **Note**: This tutorial uses manual SQL and helper functions. For the recommended approach using `defmodel` (which auto-generates CRUD functions and table schemas), see **[Tutorial: Todo API with defmodel](tutorial-defmodel.md)**.

Build a complete CRUD API with database persistence. Takes about 30 minutes.

**Prerequisites**: Complete the [Quickstart](01-quickstart.md) first.

## What You'll Build

A todo list API with these endpoints:

| Method | Path | Description |
|--------|------|-------------|
| GET | /todos | List all todos |
| GET | /todos/:id | Get single todo |
| POST | /todos | Create todo |
| PUT | /todos/:id | Update todo |
| DELETE | /todos/:id | Delete todo |

## Step 1: Project Setup

Create a new file `todo-api.lisp`:

```lisp
(defpackage :todo-api
  (:use :cl :quickapi))

(in-package :todo-api)

(defapi todo-api
  :name "Todo API"
  :version "1.0")
```

Load it:

```lisp
(load "todo-api.lisp")
```

## Step 2: Database Setup

Add database initialization:

```lisp
(defvar *db-path* "todos.db")

(defun init-database ()
  "Create the todos table if it doesn't exist."
  (with-db (*db-path*)
    (ensure-table :todos
      '((id integer :primary-key :autoincrement)
        (title text :not-null)
        (completed integer :not-null)
        (created_at text :not-null)))))
```

Run it:

```lisp
(init-database)
```

Verify: A file `todos.db` now exists in your directory.

## Step 3: Helper Functions

Add these helpers to convert database rows to JSON:

```lisp
(defun make-timestamp ()
  "Get current time as ISO string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun todo-to-hash (row)
  "Convert database row to hash-table for JSON."
  (destructuring-bind (id title completed created-at) row
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "id" h) id)
      (setf (gethash "title" h) title)
      (setf (gethash "completed" h) (= completed 1))
      (setf (gethash "created_at" h) created-at)
      h)))
```

## Step 4: List Todos (GET /todos)

Add the list endpoint:

```lisp
(api-get "/todos" ()
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos
                  :order-by '(:created_at :desc))))
      (mapcar #'todo-to-hash rows))))
```

Reload and test:

```bash
curl http://localhost:8000/todos
```

Expected output (empty list):

```json
[]
```

## Step 5: Create Todo (POST /todos)

Add the create endpoint with validation:

```lisp
(api-post "/todos" ()
  ;; Validate the request body
  (validate *body*
    (require-fields "title")
    (require-type "title" 'string)
    (require-length "title" :min 1 :max 200))

  (with-db (*db-path*)
    ;; Insert the new todo
    (sqlite:insert *db* :todos
      (list :title (gethash "title" *body*)
            :completed 0
            :created_at (make-timestamp)))

    ;; Return the created todo
    (let* ((id (last-insert-id))
           (rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (created (todo-to-hash (first rows))))))
```

Test creating a todo:

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{"title":"Buy milk"}' \
     http://localhost:8000/todos
```

Expected output:

```json
{"id":1,"title":"Buy milk","completed":false,"created_at":"2024-01-15T10:30:00"}
```

Test validation (missing title):

```bash
curl -X POST -H "Content-Type: application/json" \
     -d '{}' \
     http://localhost:8000/todos
```

Expected output:

```json
{"error":"validation_error","message":"Validation failed","details":[{"field":"title","message":"required"}]}
```

## Step 6: Get Single Todo (GET /todos/:id)

Add the get-by-id endpoint. Note that `id` is automatically bound from the `:id` path parameter:

```lisp
(api-get "/todos/:id" ()
  ;; 'id' is automatically bound from the :id path parameter
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (if rows
          (todo-to-hash (first rows))
          (not-found "Todo not found")))))
```

Test:

```bash
curl http://localhost:8000/todos/1
```

Expected output:

```json
{"id":1,"title":"Buy milk","completed":false,"created_at":"2024-01-15T10:30:00"}
```

Test not found:

```bash
curl http://localhost:8000/todos/999
```

Expected output:

```json
{"error":"not_found","message":"Todo not found"}
```

## Step 7: Update Todo (PUT /todos/:id)

Add the update endpoint. Path parameters like `id` are automatically bound:

```lisp
(api-put "/todos/:id" ()
  ;; 'id' is automatically bound from the :id path parameter
  (with-db (*db-path*)
    ;; Check todo exists
    (unless (sqlite:select *db* :todos :where `(:= :id ,id))
      (not-found "Todo not found"))

    ;; Update fields that were provided
    (let ((update-data '()))
      (when (gethash "title" *body*)
        (push (gethash "title" *body*) update-data)
        (push :title update-data))
      (when (gethash "completed" *body*)
        (push (if (gethash "completed" *body*) 1 0) update-data)
        (push :completed update-data))

      (when update-data
        (sqlite:update-table *db* :todos update-data
          :where `(:= :id ,id))))

    ;; Return updated todo
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (todo-to-hash (first rows)))))
```

Test marking as complete:

```bash
curl -X PUT -H "Content-Type: application/json" \
     -d '{"completed":true}' \
     http://localhost:8000/todos/1
```

Expected output:

```json
{"id":1,"title":"Buy milk","completed":true,"created_at":"2024-01-15T10:30:00"}
```

## Step 8: Delete Todo (DELETE /todos/:id)

Add the delete endpoint:

```lisp
(api-delete "/todos/:id" ()
  ;; 'id' is automatically bound from the :id path parameter
  (with-db (*db-path*)
    (unless (sqlite:select *db* :todos :where `(:= :id ,id))
      (not-found "Todo not found"))
    (sqlite:delete-from *db* :todos :where `(:= :id ,id))
    (no-content)))
```

Test:

```bash
curl -X DELETE http://localhost:8000/todos/1
```

Expected: Empty response with status 204.

Verify deletion:

```bash
curl http://localhost:8000/todos/1
```

Expected output:

```json
{"error":"not_found","message":"Todo not found"}
```

## Step 9: Startup Function

Add a main function:

```lisp
(defun main ()
  "Initialize and start the API."
  (init-database)
  (start :port 8000))
```

Now you can start everything with:

```lisp
(todo-api:main)
```

## Complete Code

See `examples/todo-api.lisp` for the complete implementation.

## What You Learned

- Defining routes with `api-get`, `api-post`, `api-put`, `api-delete`
- Path parameters (`:id`) are automatically bound as variables
- Accessing JSON body with `*body*`
- Validating requests with `validate` and `require-*` functions
- Using SQLite with `with-db`, `ensure-table`, and `last-insert-id`
- Returning errors with `not-found` and `bad-request`
- Returning success with `ok`, `created`, and `no-content`

## Next Steps

- [Database Guide](03-database.md) - Learn more about inquisitio
- [Deployment](04-deployment.md) - Deploy to production
