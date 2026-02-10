---
type: scenario
name: multiple-errors
feature: validation
---

# Multiple Validation Errors

## Scenario

Client sends data failing multiple validation checks.

## When

```lisp
(api-post "/users" ()
  (validate *body*
    (require-fields "name" "email")
    (require-length "password" :min 8))
  (create-user *body*))
```

Client sends: `{"password": "123"}`

## Then

Response is 422 with all errors:
```json
{
  "error": "validation_error",
  "message": "Validation failed",
  "details": [
    {"field": "name", "message": "required"},
    {"field": "email", "message": "required"},
    {"field": "password", "message": "must be at least 8 characters"}
  ]
}
```
