---
type: scenario
name: json-round-trip
feature: json_serialization
---

# JSON Round-Trip

## Scenario

Client sends JSON, server processes it, returns JSON response.

## When

```lisp
(api-post "/echo" ()
  (ok *body*))
```

## Then

Client request:
```http
POST /echo HTTP/1.1
Content-Type: application/json

{"name": "Alice", "age": 30}
```

Server response:
```http
HTTP/1.1 200 OK
Content-Type: application/json; charset=utf-8

{"name":"Alice","age":30}
```
