# sqlite_integration Feature

**Status**: stable | **Confidence**: 0.93 | **Category**: core

---

## Overview

SQLite integration provides database helpers for managing connections, creating tables, and executing queries. The with-db macro binds *db* for database operations within a lexical scope, with automatic connection cleanup.

---

## Quick Reference

### Database Operations

```lisp
;; Create table and query
(with-db (db "todos.db")
  (ensure-table *db*
    "todos"
    '(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
      ("title" "TEXT NOT NULL")
      ("completed" "BOOLEAN DEFAULT 0")))
  
  (execute *db* "INSERT INTO todos (title) VALUES (?)" "Buy milk")
  (let ((id (last-insert-rowid *db*)))
    (query *db* "SELECT * FROM todos WHERE id = ?" id)))

;; Route with database
(api-get "/todos" ()
  (with-db (db "todos.db")
    (ok :todos (query-all *db* "SELECT * FROM todos"))))

(api-post "/todos" ()
  (with-db (db "todos.db")
    (execute *db* "INSERT INTO todos (title) VALUES (?)"
             (gethash "title" *body*))
    (created :id (last-insert-rowid *db*))))
```

---

## Primary Contracts

| Contract | Purpose |
|----------|---------|
| `with-db` | Bind *db* connection to SQLite database file |
| `ensure-table` | Create table with schema if it doesn't exist |
| `query` | Execute query, return first row as hash table |
| `query-all` | Execute query, return all rows as list of hash tables |
| `execute` | Execute command (INSERT/UPDATE/DELETE), return affected rows |
| `last-insert-rowid` | Get ID of last inserted row |

---

## Core Properties

- *db* only bound within with-db lexical scope
- Connections automatically closed on scope exit
- Query results returned as hash tables (:test 'equal)
- ensure-table is idempotent (safe to call multiple times)
- Parameterized queries use ? placeholders (SQL injection prevention)

---

## Schema Format

```lisp
;; List of (column-name type-constraint) pairs
'(("id" "INTEGER PRIMARY KEY AUTOINCREMENT")
  ("title" "TEXT NOT NULL")
  ("completed" "BOOLEAN DEFAULT 0")
  ("created_at" "DATETIME DEFAULT CURRENT_TIMESTAMP"))
```

---

## Files

| File | Purpose |
|------|---------|
| `feature.yaml` | Feature metadata and specification |
| `.context.yaml` | Agent context bundle (auto-generated) |

---

## Implementation

**Source**: `src/sqlite.lisp` (all)
**Tests**: `tests/sqlite-tests.lisp` (6 comprehensive tests)
**Examples**: `examples/todo-api.lisp`

---

## Related Features

- [`request_context`](../request_context/) - *db* special variable convention
- [`json_serialization`](../json_serialization/) - Query results as hash tables (JSON-compatible)
- [`validation`](../validation/) - Validate data before database operations

---

*Part of Canon v0.2.0 | [Features Index](../INDEX.md) | [Canon Root](../../INDEX.md)*
