;;;; ABOUTME: Tests for defmodel macro and generated CRUD functions

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; Model Registry Tests

(5am:test defmodel-registers-model
  "Test that defmodel registers model metadata"
  (qa:with-db (":memory:")
    ;; Clear any existing models
    (setf qa::*models* (make-hash-table))
    ;; Define a test model
    (eval '(qa:defmodel test-item
            ((name :type string :required t)
             (price :type integer :default 0))))
    ;; Check model was registered
    (5am:is (gethash 'test-item qa::*models*))
    (let ((spec (gethash 'test-item qa::*models*)))
      (5am:is (= 2 (length (getf spec :fields)))))))

;;; Table Generation Tests

(5am:test defmodel-generates-table-columns
  "Test that model-to-columns generates correct SQLite column specs"
  (let ((fields '((:name title :type string :required t :max-length 200)
                  (:name completed :type boolean :default nil))))
    (let ((columns (qa::model-to-columns fields)))
      ;; Should have id, title, completed, created_at, updated_at
      (5am:is (= 5 (length columns)))
      ;; Check id column exists (symbol comparison)
      (5am:is (string= "ID" (symbol-name (first (first columns)))))
      ;; Check title column has NOT NULL
      (5am:is (member :not-null (find :title columns :key #'first))))))

(5am:test defmodel-creates-table
  "Test that defmodel creates the database table"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel widget
            ((name :type string :required t))))
    ;; Migrate to create table
    (qa:migrate-models)
    ;; Verify table exists by inserting
    (5am:finishes
      (sqlite:execute-non-query qa:*db*
        "INSERT INTO widgets (name) VALUES (?)" "test"))))

;;; CRUD Function Tests

(5am:test create-model-inserts-record
  "Test that create-<model> inserts a record and returns it with id"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel product
            ((name :type string :required t)
             (price :type integer :default 0))))
    (qa:migrate-models)
    (let ((result (create-product (make-hash-table-from-alist
                                   '(("name" . "Widget") ("price" . 100))))))
      (5am:is (hash-table-p result))
      (5am:is (gethash "id" result))
      (5am:is (string= "Widget" (gethash "name" result)))
      (5am:is (= 100 (gethash "price" result))))))

(5am:test find-model-by-id
  "Test that find-<model> retrieves a record by id"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel item
            ((name :type string :required t))))
    (qa:migrate-models)
    (let* ((created (create-item (make-hash-table-from-alist '(("name" . "Test")))))
           (id (gethash "id" created))
           (found (find-item id)))
      (5am:is-true found)
      (5am:is (string= "Test" (gethash "name" found))))))

(5am:test find-model-returns-nil-for-missing
  "Test that find-<model> returns nil for non-existent id"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel thing
            ((name :type string :required t))))
    (qa:migrate-models)
    (5am:is (null (handler-bind ((qa:record-not-found
                                   (lambda (c)
                                     (declare (ignore c))
                                     (invoke-restart 'qa::return-nil))))
                    (find-thing 99999))))))

(5am:test list-models-returns-all
  "Test that list-<model>s returns all records"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel task
            ((title :type string :required t))))
    (qa:migrate-models)
    (create-task (make-hash-table-from-alist '(("title" . "Task 1"))))
    (create-task (make-hash-table-from-alist '(("title" . "Task 2"))))
    (create-task (make-hash-table-from-alist '(("title" . "Task 3"))))
    (let ((all (list-tasks)))
      (5am:is (= 3 (length all))))))

(5am:test list-models-with-pagination
  "Test that list-<model>s supports limit and offset"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel entry
            ((value :type integer :required t))))
    (qa:migrate-models)
    (dotimes (i 10)
      (create-entry (make-hash-table-from-alist `(("value" . ,i)))))
    ;; Get first 3
    (let ((page1 (list-entrys :limit 3)))
      (5am:is (= 3 (length page1))))
    ;; Get next 3
    (let ((page2 (list-entrys :limit 3 :offset 3)))
      (5am:is (= 3 (length page2))))))

(5am:test update-model-changes-fields
  "Test that update-<model> modifies record fields"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel note
            ((content :type string :required t))))
    (qa:migrate-models)
    (let* ((created (create-note (make-hash-table-from-alist '(("content" . "Original")))))
           (id (gethash "id" created)))
      (update-note id (make-hash-table-from-alist '(("content" . "Updated"))))
      (let ((found (find-note id)))
        (5am:is (string= "Updated" (gethash "content" found)))))))

(5am:test delete-model-removes-record
  "Test that delete-<model> removes the record"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel record
            ((data :type string :required t))))
    (qa:migrate-models)
    (let* ((created (create-record (make-hash-table-from-alist '(("data" . "Test")))))
           (id (gethash "id" created)))
      (delete-record id)
      (5am:is (null (handler-bind ((qa:record-not-found
                                     (lambda (c)
                                       (declare (ignore c))
                                       (invoke-restart 'qa::return-nil))))
                      (find-record id)))))))

(5am:test count-models-returns-count
  "Test that count-<model>s returns the number of records"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel counter
            ((value :type integer :default 0))))
    (qa:migrate-models)
    (5am:is (= 0 (count-counters)))
    (create-counter (make-hash-table))
    (create-counter (make-hash-table))
    (5am:is (= 2 (count-counters)))))

;;; Validation Integration Tests

(5am:test create-model-validates-required-fields
  "Test that create-<model> validates required fields"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel article
            ((title :type string :required t)
             (body :type string))))
    (qa:migrate-models)
    ;; Should signal validation error when required field missing
    (5am:signals qa:validation-error
      (create-article (make-hash-table-from-alist '(("body" . "content")))))))

(5am:test create-model-validates-max-length
  "Test that create-<model> validates max-length constraint"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel post
            ((title :type string :required t :max-length 10))))
    (qa:migrate-models)
    ;; Should signal validation error when title too long
    (5am:signals qa:validation-error
      (create-post (make-hash-table-from-alist '(("title" . "this is way too long")))))))

(5am:test create-model-passes-valid-data
  "Test that create-<model> accepts valid data"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel message
            ((text :type string :required t :max-length 100))))
    (qa:migrate-models)
    ;; Should succeed with valid data
    (let ((result (create-message (make-hash-table-from-alist '(("text" . "Hello"))))))
      (5am:is-true result)
      (5am:is (string= "Hello" (gethash "text" result))))))

;;; find-by Tests

(5am:test find-by-returns-result-when-found
  "Test that find-<model>-by returns a record when field matches"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel fruit
            ((name :type string :required t :unique t)
             (color :type string :required t))))
    (qa:migrate-models)
    (create-fruit (make-hash-table-from-alist '(("name" . "apple") ("color" . "red"))))
    (let ((found (find-fruit-by :name "apple")))
      (5am:is-true found)
      (5am:is (hash-table-p found))
      (5am:is (string= "apple" (gethash "name" found)))
      (5am:is (string= "red" (gethash "color" found))))))

(5am:test find-by-returns-nil-when-not-found
  "Test that find-<model>-by returns nil (not error) when no match"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel vegetable
            ((name :type string :required t))))
    (qa:migrate-models)
    (5am:is (null (find-vegetable-by :name "nonexistent")))))

;;; list with :where Tests

(5am:test list-models-with-where
  "Test that list-<model>s supports :where filtering"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel animal
            ((species :type string :required t)
             (legs :type integer :required t))))
    (qa:migrate-models)
    (create-animal (make-hash-table-from-alist '(("species" . "cat") ("legs" . 4))))
    (create-animal (make-hash-table-from-alist '(("species" . "dog") ("legs" . 4))))
    (create-animal (make-hash-table-from-alist '(("species" . "spider") ("legs" . 8))))
    ;; Filter by legs = 4
    (let ((four-legged (list-animals :where '(:= :legs 4))))
      (5am:is (= 2 (length four-legged))))
    ;; Filter by species
    (let ((cats (list-animals :where '(:= :species "cat"))))
      (5am:is (= 1 (length cats)))
      (5am:is (string= "cat" (gethash "species" (first cats)))))))

;;; count with :where Tests

(5am:test count-models-with-where
  "Test that count-<model>s supports :where filtering"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    (eval '(qa:defmodel color
            ((name :type string :required t)
             (warm :type boolean :default nil))))
    (qa:migrate-models)
    (create-color (make-hash-table-from-alist '(("name" . "red") ("warm" . t))))
    (create-color (make-hash-table-from-alist '(("name" . "orange") ("warm" . t))))
    (create-color (make-hash-table-from-alist '(("name" . "blue") ("warm" . nil))))
    ;; Total count
    (5am:is (= 3 (count-colors)))
    ;; Filtered count (warm=1 because booleans are stored as integers)
    (5am:is (= 2 (count-colors :where '(:= :warm 1))))))

;;; Auto-export Tests

(5am:test defmodel-exports-generated-functions
  "Test that defmodel auto-exports all generated CRUD functions"
  (qa:with-db (":memory:")
    (setf qa::*models* (make-hash-table))
    ;; Define model in quickapi/tests package
    (eval '(qa:defmodel gadget
            ((name :type string :required t))))
    ;; Check that functions are exported from the current package
    (let ((pkg (find-package :quickapi/tests)))
      (dolist (sym-name '("CREATE-GADGET" "FIND-GADGET" "FIND-GADGET-BY"
                          "LIST-GADGETS" "UPDATE-GADGET" "DELETE-GADGET"
                          "COUNT-GADGETS"))
        (multiple-value-bind (sym status) (find-symbol sym-name pkg)
          (5am:is-true sym (format nil "~a should exist" sym-name))
          (5am:is (eq status :external)
                  (format nil "~a should be external" sym-name)))))))

;;; Test Helper

(defun make-hash-table-from-alist (alist)
  "Create a hash table from an alist."
  (let ((ht (make-hash-table :test 'equal)))
    (dolist (pair alist ht)
      (setf (gethash (car pair) ht) (cdr pair)))))
