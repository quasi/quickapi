---
type: contract
name: database-helpers
version: 1.0.0
feature: sqlite_integration
---

# SQLite Database Helpers

## Purpose

Provide thin wrappers over cl-sqlite for common database operations.

## Interface

```lisp
(with-db (path) &body body)
(ensure-table name columns)
(last-insert-id)
(row-to-hash row column-names)
(rows-to-json rows &optional column-names)
(with-transaction &body body)
```

## Requirements

**R1**: `with-db` MUST bind `*db*` to an open connection.

**R2**: `ensure-table` MUST be idempotent (safe to call multiple times).

**R3**: `with-transaction` MUST commit on success, rollback on error.

**R4**: Helper functions MUST work with cl-sqlite/inquisitio underneath.

## Guarantees

**G1**: Database connections are properly closed even on error.

**G2**: `row-to-hash` produces hash-tables suitable for JSON serialization.

**G3**: Transactions provide ACID guarantees via SQLite.
