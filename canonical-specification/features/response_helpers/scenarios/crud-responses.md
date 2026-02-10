---
type: scenario
name: crud-responses
feature: response_helpers
---

# CRUD Response Patterns

## Scenario

Use appropriate response helpers for each CRUD operation.

## When

```lisp
(api-get "/items" ()
  (ok (list-items)))  ; 200 OK

(api-post "/items" ()
  (created (create-item *body*)))  ; 201 Created

(api-get "/items/:id" ()
  (let ((item (find-item id)))
    (if item
        (ok item)
        (not-found "Item not found"))))  ; 404

(api-delete "/items/:id" ()
  (delete-item id)
  (no-content))  ; 204 No Content
```

## Then

Each operation returns semantically correct status code and response format.
