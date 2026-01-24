# Database Guide

Using SQLite with quickapi via cl-sqlite.

**Prerequisites**: [Quickstart](01-quickstart.md)

## Overview

Quickapi includes conveniences for cl-sqlite, a Common Lisp SQLite library. SQLite is ideal for small to medium APIs:

- Zero configuration
- Single file database
- No separate server process
- Sufficient for most API workloads

## Connection Management

Use `with-db` to establish a database connection:

```lisp
(with-db ("myapp.db")
  ;; *db* is bound to the connection here
  (sqlite:select *db* :users))
```

For in-memory databases (testing):

```lisp
(with-db (":memory:")
  ;; Database exists only for this block
  ...)
```

## Creating Tables

Use `ensure-table` to create tables idempotently:

```lisp
(with-db ("myapp.db")
  (ensure-table :users
    '((id integer :primary-key :autoincrement)
      (email text :not-null :unique)
      (name text :not-null)
      (created_at text :not-null))))
```

Column options:

| Option | SQL Result |
|--------|------------|
| `:primary-key` | PRIMARY KEY |
| `:autoincrement` | AUTOINCREMENT |
| `:not-null` | NOT NULL |
| `:unique` | UNIQUE |

Column types: `integer`, `text`, `real`, `blob`

## CRUD Operations

### Insert

```lisp
(sqlite:insert *db* :users
  (list :email "alice@example.com"
        :name "Alice"
        :created_at "2024-01-15"))
```

Get the inserted row's ID:

```lisp
(let ((id (last-insert-id)))
  ...)
```

### Select

Basic select:

```lisp
;; All rows
(sqlite:select *db* :users)

;; With WHERE clause
(sqlite:select *db* :users :where '(:= :id 1))

;; Multiple conditions
(sqlite:select *db* :users
  :where '(:and (:= :active 1)
                (:> :age 18)))
```

WHERE clause operators:

| Operator | Example | SQL |
|----------|---------|-----|
| `:=` | `(:= :id 1)` | `id = 1` |
| `:<` | `(:< :age 30)` | `age < 30` |
| `:>` | `(:> :price 100)` | `price > 100` |
| `:<=` | `(:<= :qty 10)` | `qty <= 10` |
| `:>=` | `(:>= :rating 4)` | `rating >= 4` |
| `:<>` | `(:<> :status "deleted")` | `status <> 'deleted'` |
| `:like` | `(:like :name "A%")` | `name LIKE 'A%'` |
| `:in` | `(:in :status "a" "b")` | `status IN ('a', 'b')` |
| `:is-null` | `(:is-null :deleted_at)` | `deleted_at IS NULL` |
| `:is-not-null` | `(:is-not-null :email)` | `email IS NOT NULL` |
| `:and` | `(:and c1 c2)` | `(c1) AND (c2)` |
| `:or` | `(:or c1 c2)` | `(c1) OR (c2)` |
| `:not` | `(:not c1)` | `NOT (c1)` |

Ordering and pagination:

```lisp
(sqlite:select *db* :users
  :order-by '(:created_at :desc)
  :limit 10
  :offset 20)
```

Specific columns:

```lisp
(sqlite:select *db* :users
  :columns '(:id :name))
```

### Update

```lisp
(sqlite:update-table *db* :users
  (list :name "Alice Smith")
  :where '(:= :id 1))
```

### Delete

```lisp
(sqlite:delete-from *db* :users
  :where '(:= :id 1))
```

## Transactions

Wrap multiple operations in a transaction:

```lisp
(with-db ("myapp.db")
  (with-transaction
    (sqlite:insert *db* :accounts
      (list :name "Alice" :balance 1000))
    (sqlite:insert *db* :accounts
      (list :name "Bob" :balance 500))))
```

If any operation fails, all changes are rolled back.

## Converting Results to JSON

Database rows come back as lists. Convert them to hash-tables for JSON:

```lisp
(defun user-to-hash (row)
  (destructuring-bind (id email name created-at) row
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "id" h) id)
      (setf (gethash "email" h) email)
      (setf (gethash "name" h) name)
      (setf (gethash "created_at" h) created-at)
      h)))

(api-get "/users" ()
  (with-db ("myapp.db")
    (mapcar #'user-to-hash
            (sqlite:select *db* :users))))
```

## Raw SQL

For complex queries, use raw SQL:

```lisp
(sqlite:execute-to-list *db*
  "SELECT u.*, COUNT(p.id) as post_count
   FROM users u
   LEFT JOIN posts p ON p.user_id = u.id
   GROUP BY u.id")
```

With parameters (prevents SQL injection):

```lisp
(sqlite:execute-to-list *db*
  "SELECT * FROM users WHERE email = ?"
  "alice@example.com")
```

## Testing

Use in-memory databases for tests:

```lisp
(defun test-user-creation ()
  (with-db (":memory:")
    (ensure-table :users
      '((id integer :primary-key :autoincrement)
        (name text :not-null)))
    (sqlite:insert *db* :users (list :name "Test"))
    (let ((users (sqlite:select *db* :users)))
      (assert (= 1 (length users))))))
```

## Troubleshooting

**Error**: `database is locked`

**Cause**: Another process has the database open.

**Fix**: SQLite allows one writer at a time. Ensure you're not running multiple instances writing to the same file.

---

**Error**: `no such table`

**Cause**: Table doesn't exist.

**Fix**: Call `ensure-table` before queries, or check your database file path.

---

**Error**: `UNIQUE constraint failed`

**Cause**: Inserting duplicate value in a unique column.

**Fix**: Check if record exists first, or handle the `sqlite-constraint-error` condition.

## Next Steps

- [Deployment](04-deployment.md) - Deploy your API to production
