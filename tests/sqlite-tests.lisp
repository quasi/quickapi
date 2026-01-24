;;;; ABOUTME: Tests for SQLite conveniences

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; Database Setup Fixture

(defun setup-test-db ()
  "Setup in-memory test database."
  (qa:with-db (":memory:")
    (qa::ensure-table :test_users
      '((id integer :primary-key :autoincrement)
        (name text :not-null)
        (email text :not-null)))
    qa:*db*))

;;; WITH-DB Tests

(5am:test with-db-opens-connection
  "Test WITH-DB opens and closes database connection"
  (5am:finishes
    (qa:with-db (":memory:")
      (5am:is (not (null qa:*db*))))))

(5am:test with-db-scoped-binding
  "Test *DB* is only bound within WITH-DB"
  (let ((outer-db qa:*db*))
    (qa:with-db (":memory:")
      (5am:is (not (null qa:*db*))))
    (5am:is (eq outer-db qa:*db*))))

;;; ENSURE-TABLE Tests

(5am:test ensure-table-creates-table
  "Test ENSURE-TABLE creates a new table"
  (5am:finishes
    (qa:with-db (":memory:")
      (qa::ensure-table :items
        '((id integer :primary-key :autoincrement)
          (name text :not-null))))))

(5am:test ensure-table-idempotent
  "Test ENSURE-TABLE is idempotent"
  (5am:finishes
    (qa:with-db (":memory:")
      (qa::ensure-table :items
        '((id integer :primary-key :autoincrement)
          (name text :not-null)))
      ;; Should not error when called again
      (qa::ensure-table :items
        '((id integer :primary-key :autoincrement)
          (name text :not-null))))))

;;; LAST-INSERT-ID Tests

(5am:test last-insert-id-returns-rowid
  "Test LAST-INSERT-ID returns correct row ID after insert"
  (qa:with-db (":memory:")
    (qa::ensure-table :items
      '((id integer :primary-key :autoincrement)
        (name text :not-null)))
    (sqlite:execute-non-query qa:*db*
      "INSERT INTO items (name) VALUES (?)" "test")
    (let ((id (qa::last-insert-id)))
      (5am:is (= 1 id)))))

(5am:test last-insert-id-sequential
  "Test LAST-INSERT-ID increments with multiple inserts"
  (qa:with-db (":memory:")
    (qa::ensure-table :items
      '((id integer :primary-key :autoincrement)
        (name text :not-null)))
    (sqlite:execute-non-query qa:*db*
      "INSERT INTO items (name) VALUES (?)" "first")
    (5am:is (= 1 (qa::last-insert-id)))
    (sqlite:execute-non-query qa:*db*
      "INSERT INTO items (name) VALUES (?)" "second")
    (5am:is (= 2 (qa::last-insert-id)))))

;;; ROW-TO-HASH Tests

(5am:test row-to-hash-with-column-names
  "Test ROW-TO-HASH converts row with column names"
  (let ((row '(1 "Alice" "alice@example.com"))
        (columns '(:id :name :email)))
    (let ((result (qa::row-to-hash row columns)))
      (5am:is (= 1 (gethash "id" result)))
      (5am:is (string= "Alice" (gethash "name" result)))
      (5am:is (string= "alice@example.com" (gethash "email" result))))))

(5am:test row-to-hash-without-column-names
  "Test ROW-TO-HASH uses indices without column names"
  (let ((row '(1 "Alice" "alice@example.com")))
    (let ((result (qa::row-to-hash row nil)))
      (5am:is (= 1 (gethash 0 result)))
      (5am:is (string= "Alice" (gethash 1 result)))
      (5am:is (string= "alice@example.com" (gethash 2 result))))))

;;; ROWS-TO-JSON Tests

(5am:test rows-to-json-multiple-rows
  "Test ROWS-TO-JSON converts multiple rows"
  (let ((rows '((1 "Alice" "alice@example.com")
                (2 "Bob" "bob@example.com")))
        (columns '(:id :name :email)))
    (let ((result (qa::rows-to-json rows columns)))
      (5am:is (= 2 (length result)))
      (5am:is (= 1 (gethash "id" (first result))))
      (5am:is (string= "Alice" (gethash "name" (first result))))
      (5am:is (= 2 (gethash "id" (second result))))
      (5am:is (string= "Bob" (gethash "name" (second result)))))))

(5am:test rows-to-json-empty
  "Test ROWS-TO-JSON handles empty result"
  (let ((result (qa::rows-to-json '() '(:id :name))))
    (5am:is (null result))))

;;; WITH-TRANSACTION Tests

(5am:test with-transaction-commits
  "Test WITH-TRANSACTION commits on success"
  (qa:with-db (":memory:")
    (qa::ensure-table :items
      '((id integer :primary-key :autoincrement)
        (name text :not-null)))
    (qa::with-transaction
      (sqlite:execute-non-query qa:*db*
        "INSERT INTO items (name) VALUES (?)" "test"))
    ;; Verify data persists
    (let ((rows (sqlite:execute-to-list qa:*db* "SELECT * FROM items")))
      (5am:is (= 1 (length rows))))))

(5am:test with-transaction-rolls-back
  "Test WITH-TRANSACTION rolls back on error"
  (qa:with-db (":memory:")
    (qa::ensure-table :items
      '((id integer :primary-key :autoincrement)
        (name text :not-null)))
    ;; Start transaction and insert, then error
    (ignore-errors
      (qa::with-transaction
        (sqlite:execute-non-query qa:*db*
          "INSERT INTO items (name) VALUES (?)" "test")
        (error "Intentional error")))
    ;; Verify data was rolled back
    (let ((rows (sqlite:execute-to-list qa:*db* "SELECT * FROM items")))
      (5am:is (= 0 (length rows))))))

;;; Integration Test

(5am:test sqlite-full-crud-workflow
  "Test complete CRUD workflow with SQLite helpers"
  (qa:with-db (":memory:")
    ;; Create table
    (qa::ensure-table :users
      '((id integer :primary-key :autoincrement)
        (name text :not-null)
        (email text :not-null)))

    ;; Insert
    (sqlite:execute-non-query qa:*db*
      "INSERT INTO users (name, email) VALUES (?, ?)"
      "Alice" "alice@example.com")
    (let ((id (qa::last-insert-id)))
      (5am:is (= 1 id))

      ;; Read
      (let* ((rows (sqlite:execute-to-list qa:*db*
                     "SELECT * FROM users WHERE id = ?" id))
             (hash (qa::row-to-hash (first rows) '(:id :name :email))))
        (5am:is (= 1 (gethash "id" hash)))
        (5am:is (string= "Alice" (gethash "name" hash))))

      ;; Update
      (sqlite:execute-non-query qa:*db*
        "UPDATE users SET name = ? WHERE id = ?" "Alice Smith" id)
      (let* ((rows (sqlite:execute-to-list qa:*db*
                     "SELECT name FROM users WHERE id = ?" id)))
        (5am:is (string= "Alice Smith" (first (first rows)))))

      ;; Delete
      (sqlite:execute-non-query qa:*db*
        "DELETE FROM users WHERE id = ?" id)
      (let ((rows (sqlite:execute-to-list qa:*db* "SELECT * FROM users")))
        (5am:is (= 0 (length rows)))))))
