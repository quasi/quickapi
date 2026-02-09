;;;; ABOUTME: defmodel macro for declarative database models with auto-generated CRUD

(in-package :quickapi)

;;; Model Registry
;;; Stores metadata about defined models for migration and introspection

(defvar *models* (make-hash-table)
  "Registry of defined models. Maps model name (symbol) to model spec (plist).")

;;; Type Mapping
;;; Maps quickapi types to SQLite types and provides conversion functions

(defparameter *type-map*
  '((string   :sqlite "TEXT"    :cl-type string    :json-type string)
    (integer  :sqlite "INTEGER" :cl-type integer   :json-type number)
    (boolean  :sqlite "INTEGER" :cl-type boolean   :json-type boolean)
    (float    :sqlite "REAL"    :cl-type float     :json-type number)
    (datetime :sqlite "TEXT"    :cl-type string    :json-type string)
    (json     :sqlite "TEXT"    :cl-type hash-table :json-type object))
  "Type mapping between quickapi types, SQLite types, and JSON types.")

(defun sqlite-type (quickapi-type)
  "Get the SQLite type for a quickapi type."
  (let ((mapping (assoc quickapi-type *type-map*)))
    (if mapping
        (getf (cdr mapping) :sqlite)
        "TEXT")))

;;; Field Parsing

(defun parse-field-spec (field-spec)
  "Parse a field specification into a normalized plist.
   Input: (name :type string :required t :max-length 200)
   Output: (:name name :type string :required t :max-length 200 ...)"
  (destructuring-bind (name &rest options) field-spec
    (list* :name name options)))

(defun parse-fields (field-specs)
  "Parse all field specifications."
  (mapcar #'parse-field-spec field-specs))

;;; Model Name Utilities

(defun pluralize (name)
  "Simple pluralization for table names.
   todo -> todos, user -> users"
  (let ((name-str (string-downcase (symbol-name name))))
    (cond
      ((cl-ppcre:scan "s$" name-str) (concatenate 'string name-str "es"))
      ((cl-ppcre:scan "y$" name-str)
       (concatenate 'string (subseq name-str 0 (1- (length name-str))) "ies"))
      (t (concatenate 'string name-str "s")))))

(defun model-table-name (model-name)
  "Get the table name for a model."
  (pluralize model-name))

(defun lisp-to-sql-name (name)
  "Convert a Lisp name (with hyphens) to SQL name (with underscores).
   E.g., password-hash -> password_hash"
  (substitute #\_ #\- (string-downcase (if (symbolp name)
                                            (symbol-name name)
                                            name))))

;;; Table DDL Generation

(defun field-to-column (field-spec)
  "Convert a parsed field spec to a SQLite column definition.
   Input: (:name title :type string :required t :default \"untitled\")
   Output: (title_col text :not-null :default \"untitled\")
   Note: Converts hyphens to underscores for SQL compatibility."
  (let* ((name (getf field-spec :name))
         (sql-name (intern (string-upcase (lisp-to-sql-name name)) :keyword))
         (type (getf field-spec :type))
         (required (getf field-spec :required))
         (default (getf field-spec :default))
         (unique (getf field-spec :unique))
         (sqlite-type (sqlite-type type))
         (column (list sql-name (intern (string-upcase sqlite-type) :keyword))))
    ;; Add constraints
    (when required
      (setf column (append column '(:not-null))))
    (when unique
      (setf column (append column '(:unique))))
    (when default
      (setf column (append column (list :default (convert-default-value default type)))))
    column))

(defun convert-default-value (value type)
  "Convert a Lisp default value to SQLite representation."
  (case type
    (boolean (if value 1 0))
    (t value)))

(defun model-to-columns (fields)
  "Generate SQLite column definitions from parsed fields.
   Automatically adds: id (primary key), created_at, updated_at"
  (let ((columns '()))
    ;; Primary key
    (push '(id integer :primary-key :autoincrement) columns)
    ;; User-defined fields
    (dolist (field fields)
      (push (field-to-column field) columns))
    ;; Timestamps with SQLite datetime defaults
    (push '(created_at text :not-null :default current_timestamp) columns)
    (push '(updated_at text :not-null :default current_timestamp) columns)
    (nreverse columns)))

;;; Migration

(defun migrate-models ()
  "Create or update tables for all registered models.
   Safe to call multiple times - uses CREATE TABLE IF NOT EXISTS."
  (maphash (lambda (name spec)
             (declare (ignore name))
             (let ((table (getf spec :table))
                   (fields (getf spec :fields)))
               (ensure-table (intern (string-upcase table) :keyword)
                             (model-to-columns fields))))
           *models*))

;;; defmodel Macro

(defmacro defmodel (name fields)
  "Define a database model with auto-generated CRUD operations.

   Syntax:
     (defmodel <name>
       ((<field> :type <type>
                 [:required t/nil]
                 [:default <value>]
                 [:unique t/nil]
                 [:max-length <n>]
                 [:min-length <n>]
                 [:pattern <regex>]
                 [:references <model>]
                 [:on-delete :cascade/:set-null/:restrict])
        ...))

   Supported types: string, integer, boolean, float, datetime, json

   Generates:
     - create-<name> (data) - Insert record, return with id
     - find-<name> (id) - Find by primary key
     - find-<name>-by (field value) - Find by any field
     - list-<name>s (&key limit offset where order-by) - List with pagination
     - update-<name> (id data) - Update fields
     - delete-<name> (id) - Delete by id
     - count-<name>s (&key where) - Count records

   Example:
     (defmodel todo
       ((title :type string :required t :max-length 200)
        (completed :type boolean :default nil)))"
  (let* ((parsed-fields (parse-fields fields))
         (table-name (model-table-name name))
         (field-names (mapcar (lambda (f) (getf f :name)) parsed-fields))
         ;; Function names
         (create-fn (intern (format nil "CREATE-~a" name)))
         (find-fn (intern (format nil "FIND-~a" name)))
         (find-by-fn (intern (format nil "FIND-~a-BY" name)))
         (list-fn (intern (format nil "LIST-~aS" name)))
         (update-fn (intern (format nil "UPDATE-~a" name)))
         (delete-fn (intern (format nil "DELETE-~a" name)))
         (count-fn (intern (format nil "COUNT-~aS" name))))
    `(progn
       ;; Register model metadata
       (setf (gethash ',name *models*)
             (list :name ',name
                   :table ,table-name
                   :fields ',parsed-fields))

       ;; create-<model>
       (defun ,create-fn (data)
         ,(format nil "Create a new ~a record. DATA is a hash-table with field values.~%Returns the created record with id. Validates against model constraints." name)
         (validate-model-data ',parsed-fields data)
         (model-create ,table-name ',field-names data))

       ;; find-<model>
       (defun ,find-fn (id)
         ,(format nil "Find a ~a by its id. Returns hash-table or nil." name)
         (model-find ,table-name id))

       ;; find-<model>-by
       (defun ,find-by-fn (field value)
         ,(format nil "Find a ~a by any field. Returns hash-table or nil." name)
         (model-find-by ,table-name field value))

       ;; list-<model>s
       (defun ,list-fn (&key limit offset order-by)
         ,(format nil "List all ~a records with optional pagination." name)
         (model-list ,table-name :limit limit :offset offset :order-by order-by))

       ;; update-<model>
       (defun ,update-fn (id data)
         ,(format nil "Update a ~a record by id. DATA is a hash-table with fields to update." name)
         (model-update ,table-name id data))

       ;; delete-<model>
       (defun ,delete-fn (id)
         ,(format nil "Delete a ~a record by id." name)
         (model-delete ,table-name id))

       ;; count-<model>s
       (defun ,count-fn ()
         ,(format nil "Count all ~a records." name)
         (model-count ,table-name))

       ',name)))

;;; Model Validation

(defun validate-model-data (fields data)
  "Validate DATA against model FIELDS constraints.
   Signals validation-error if validation fails."
  (let ((*validation-errors* nil))
    (dolist (field fields)
      (let* ((name (getf field :name))
             (key (string-downcase (symbol-name name)))
             (value (gethash key data))
             (required (getf field :required))
             (max-length (getf field :max-length))
             (min-length (getf field :min-length))
             (pattern (getf field :pattern)))
        ;; Check required
        (when (and required (or (null value)
                                (and (stringp value) (string= value ""))))
          (add-validation-error key "required"))
        ;; Check max-length
        (when (and max-length value (stringp value) (> (length value) max-length))
          (add-validation-error key
            (format nil "must be at most ~a characters" max-length)))
        ;; Check min-length
        (when (and min-length value (stringp value) (< (length value) min-length))
          (add-validation-error key
            (format nil "must be at least ~a characters" min-length)))
        ;; Check pattern
        (when (and pattern value (stringp value)
                   (not (cl-ppcre:scan pattern value)))
          (add-validation-error key "invalid format"))))
    ;; Signal if errors
    (when *validation-errors*
      (error 'validation-error :errors (nreverse *validation-errors*)))))

;;; Generic CRUD Implementation

(defun collect-insert-fields (field-names data)
  "Extract columns, values, and placeholders from DATA for INSERT.
   Returns (values columns values placeholders) as lists ready for SQL building."
  (let ((columns '())
        (values '())
        (placeholders '()))
    (dolist (field field-names)
      (let* ((lisp-key (string-downcase (symbol-name field)))
             (sql-col (lisp-to-sql-name field)))
        (multiple-value-bind (value present)
            (gethash lisp-key data)
          (when present
            (push sql-col columns)
            (push (lisp-to-sqlite-value value) values)
            (push "?" placeholders)))))
    (values (nreverse columns) (nreverse values) (nreverse placeholders))))

(defun lisp-to-sqlite-value (value)
  "Convert a Lisp value to a SQLite-compatible value.
   Booleans are converted to 0/1, others pass through unchanged."
  (cond
    ((eq value t) 1)
    ((eq value nil) 0)
    ((and (typep value 'symbol) (eq value 'false)) 0)
    (t value)))

(defun get-column-names (table-name)
  "Get column names for a table from SQLite."
  (let ((rows (sqlite:execute-to-list *db*
                (format nil "PRAGMA table_info(~a)" table-name))))
    (mapcar #'second rows)))

(defun model-create (table-name field-names data)
  "Insert a new record into TABLE-NAME using DATA hash-table.
   Field names are converted from Lisp style (hyphens) to SQL style (underscores)."
  (multiple-value-bind (columns values placeholders)
      (collect-insert-fields field-names data)
    ;; Build and execute INSERT
    (if columns
        (let ((sql (format nil "INSERT INTO ~a (~{~a~^, ~}) VALUES (~{~a~^, ~})"
                           table-name columns placeholders)))
          (apply #'sqlite:execute-non-query *db* sql values))
        ;; No columns provided - insert with defaults only
        (sqlite:execute-non-query *db*
          (format nil "INSERT INTO ~a DEFAULT VALUES" table-name)))
    ;; Return created record with id
    (model-find table-name (last-insert-id))))

(defun model-find (table-name id)
  "Find a record by id in TABLE-NAME."
  (let* ((columns (get-column-names table-name))
         (rows (sqlite:execute-to-list *db*
                 (format nil "SELECT * FROM ~a WHERE id = ?" table-name)
                 id)))
    (when rows
      (row-to-hash (first rows) columns))))

(defun model-find-by (table-name field value)
  "Find a record by FIELD=VALUE in TABLE-NAME.
   FIELD is converted from Lisp style (hyphens) to SQL style (underscores)."
  (let* ((sql-field (lisp-to-sql-name field))
         (columns (get-column-names table-name))
         (rows (sqlite:execute-to-list *db*
                 (format nil "SELECT * FROM ~a WHERE ~a = ? LIMIT 1"
                         table-name sql-field)
                 value)))
    (when rows
      (row-to-hash (first rows) columns))))

(defun model-list (table-name &key limit offset order-by)
  "List records from TABLE-NAME with optional pagination."
  (let* ((columns (get-column-names table-name))
         (sql (format nil "SELECT * FROM ~a~@[ ORDER BY ~a~]~@[ LIMIT ~a~]~@[ OFFSET ~a~]"
                      table-name order-by limit offset))
         (rows (sqlite:execute-to-list *db* sql)))
    (mapcar (lambda (row) (row-to-hash row columns)) rows)))

(defun model-update (table-name id data)
  "Update record ID in TABLE-NAME with fields from DATA hash-table.
   Field names are converted from Lisp style (hyphens) to SQL style (underscores)."
  (let ((sets '())
        (values '()))
    ;; Build SET clauses
    (maphash (lambda (key value)
               (unless (string= key "id")  ; Don't update id
                 (let ((sql-col (lisp-to-sql-name key)))
                   (push (format nil "~a = ?" sql-col) sets)
                   (push (lisp-to-sqlite-value value) values))))
             data)
    ;; Add updated_at
    (push "updated_at = datetime('now')" sets)
    ;; Build and execute UPDATE
    (when sets
      (let ((sql (format nil "UPDATE ~a SET ~{~a~^, ~} WHERE id = ?"
                         table-name (nreverse sets))))
        (apply #'sqlite:execute-non-query *db* sql (append (nreverse values) (list id)))))
    ;; Return updated record
    (model-find table-name id)))

(defun model-delete (table-name id)
  "Delete record ID from TABLE-NAME."
  (sqlite:execute-non-query *db*
    (format nil "DELETE FROM ~a WHERE id = ?" table-name)
    id)
  t)

(defun model-count (table-name)
  "Count records in TABLE-NAME."
  (let ((result (sqlite:execute-single *db*
                  (format nil "SELECT COUNT(*) FROM ~a" table-name))))
    result))
