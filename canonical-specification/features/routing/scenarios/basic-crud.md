---
type: scenario
name: basic-crud
feature: routing
---

# Basic CRUD Routes

## Scenario

Define a complete CRUD API for a "todos" resource using route macros.

## Given

```lisp
(defapi todo-api :version "1.0")
```

## When

```lisp
(api-get "/todos" ()
  (list-all-todos))

(api-get "/todos/:id" ()
  (find-todo id))  ; id automatically bound

(api-post "/todos" ()
  (create-todo *body*))

(api-put "/todos/:id" ()
  (update-todo id *body*))

(api-delete "/todos/:id" ()
  (delete-todo id)
  (no-content))
```

## Then

- GET /todos returns all todos
- GET /todos/123 binds `id` to "123"
- POST /todos receives JSON in `*body*`
- PUT /todos/456 binds `id` to "456" and receives JSON
- DELETE /todos/789 binds `id` and returns 204
