;;;; ABOUTME: SQLite conveniences for quickapi - thin wrapper over cl-sqlite

(in-package :quickapi)

;;; Database Connection Management

(defmacro with-db ((path) &body body)
  "Execute BODY with *DB* bound to an open database connection.
   PATH is the database file path, or \":memory:\" for in-memory database.

   Example:
     (with-db (\"app.db\")
       (sqlite:select *db* :users))"
  `(sqlite:with-open-database (*db* ,path)
     ,@body))

;;; Convenience Functions
;;; These re-export cl-sqlite functions and add some quickapi-specific helpers

(defun last-insert-id ()
  "Get the rowid of the last inserted row.
   Must be called within a WITH-DB block."
  (sqlite:last-insert-rowid *db*))

(defun ensure-table (name columns)
  "Create table if it doesn't exist.
   NAME is a keyword or string.
   COLUMNS is a list of column definitions.

   Example:
     (ensure-table :todos
       '((id integer :primary-key :autoincrement)
         (title text :not-null)
         (completed integer :not-null)))"
  (sqlite:create-table *db* name columns :if-not-exists t))

;;; Result Conversion for JSON Responses

(defun rows-to-json (rows &optional column-names)
  "Convert database rows to a list of hash-tables suitable for JSON.
   ROWS is a list of lists (from sqlite:select).
   COLUMN-NAMES, if provided, are used as hash keys."
  (mapcar (lambda (row)
            (row-to-hash row column-names))
          rows))

(defun row-to-hash (row column-names)
  "Convert a single row to a hash-table.
   COLUMN-NAMES provides the keys; defaults to integers if not provided."
  (let ((hash (make-hash-table :test 'equal)))
    (loop for value in row
          for i from 0
          for key = (if column-names
                        (elt column-names i)
                        i)
          do (setf (gethash (if (symbolp key)
                                (string-downcase (symbol-name key))
                                key)
                            hash)
                   value))
    hash))

;;; Transaction Helper

(defmacro with-transaction (&body body)
  "Execute BODY within a database transaction.
   Commits on success, rolls back on error.

   Example:
     (with-transaction
       (sqlite:insert *db* :accounts (list :name \"Alice\" :balance 100))
       (sqlite:insert *db* :accounts (list :name \"Bob\" :balance 200)))"
  `(sqlite:with-transaction *db*
     ,@body))
