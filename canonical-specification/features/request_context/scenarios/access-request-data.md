---
type: scenario
name: access-request-data
feature: request_context
---

# Access Request Data

## Scenario

Handler accesses request context via special variables.

## When

```lisp
(api-post "/analyze" ()
  (let ((method (getf *request* :request-method))
        (user-agent (getf *request* :http-user-agent))
        (data *body*))
    (ok :method method
        :agent user-agent
        :received data)))
```

## Then

Variables are automatically bound:
- `*request*` contains full Lack environment
- `*body*` contains parsed JSON hash-table
- No explicit binding code needed
