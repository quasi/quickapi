---
type: scenario
name: not-found-handling
feature: error_handling
---

# Not Found Handling

## Scenario

Handler signals not-found condition when resource doesn't exist.

## When

```lisp
(api-get "/users/:id" ()
  (let ((user (find-user id)))
    (if user
        (ok user)
        (not-found "User not found"))))
```

Client requests: `GET /users/999`

## Then

Response is 404 with structured error:
```json
{
  "error": "not_found",
  "message": "User not found"
}
```

Status code: 404 Not Found
Content-Type: application/json; charset=utf-8
