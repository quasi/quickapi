---
type: scenario
name: basic-persistence
feature: sqlite_integration
---

# Basic Persistence

## Scenario

Store and retrieve data using SQLite helpers.

## When

```lisp
(api-post "/todos" ()
  (with-db ("todos.db")
    (sqlite:insert *db* :todos
      (list :title (gethash "title" *body*)
            :completed 0))
    (created (row-to-hash
              (first (sqlite:select *db* :todos
                       :where `(:= :id ,(last-insert-id))))
              '("id" "title" "completed")))))
```

## Then

- Data persists to `todos.db` file
- Response includes inserted row with generated ID
- Subsequent requests can query the data
