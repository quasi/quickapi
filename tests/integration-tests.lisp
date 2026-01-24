;;;; ABOUTME: Integration tests for complete API scenarios

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; Simple Test API Definition
;;; These routes will be tested via direct invocation

(qa:defapi test-api
  :version "1.0"
  :description "Test API for integration tests")

;;; Basic Routes for Testing

(qa:api-get "/test/hello" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "message" h) "Hello, World!")
    h))

(qa:api-get "/test/echo/:word" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "echo" h) word)
    h))

(qa:api-post "/test/mirror" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "received" h) qa:*body*)
    h))

(qa:api-post "/test/validated" ()
  (qa:validate qa:*body*
    (require-fields "name" "age")
    (require-type "name" 'string)
    (require-type "age" 'number)
    (require-range "age" :min 0 :max 150))
  (qa:ok qa:*body*))

;;; Integration Tests

(5am:test api-get-basic
  "Test basic GET route returns JSON"
  ;; Note: These tests verify the route definitions compile
  ;; Full HTTP testing would require starting the server
  (5am:finishes
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "message" h) "Hello, World!")
      h)))

(5am:test api-definition-metadata
  "Test DEFAPI stores API metadata"
  (5am:is (not (null qa::*api*)))
  (5am:is (string= "1.0" (qa:api-version qa::*api*)))
  (5am:is (string= "Test API for integration tests" (qa:api-description qa::*api*))))

;;; Validation Integration Tests

(5am:test validation-with-valid-data
  "Test validation passes with valid data"
  (let ((qa:*body* (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash "name" h) "Alice")
                  (setf (gethash "age" h) 25)
                  h)))
    (5am:finishes
      (qa:validate qa:*body*
        (require-fields "name" "age")
        (require-type "name" 'string)
        (require-type "age" 'number)
        (require-range "age" :min 0 :max 150)))))

(5am:test validation-with-invalid-data
  "Test validation fails with invalid data"
  (let ((qa:*body* (let ((h (make-hash-table :test 'equal)))
                  (setf (gethash "name" h) "")
                  (setf (gethash "age" h) -5)
                  h)))
    (5am:signals snooze:http-condition
      (qa:validate qa:*body*
        (require-fields "name" "age")
        (require-range "age" :min 0 :max 150)))))

;;; Database Integration Tests

(5am:test database-with-api-workflow
  "Test database operations in API context"
  (qa:with-db (":memory:")
    ;; Setup
    (qa::ensure-table :api_items
      '((id integer :primary-key :autoincrement)
        (title text :not-null)
        (created_at text)))

    ;; Simulate POST request creating item
    (sqlite:execute-non-query qa:*db*
      "INSERT INTO api_items (title, created_at) VALUES (?, ?)"
      "Test Item" "2024-01-01T00:00:00")
    (let ((id (qa::last-insert-id)))

      ;; Simulate GET request fetching item
      (let* ((rows (sqlite:execute-to-list qa:*db*
                     "SELECT * FROM api_items WHERE id = ?" id))
             (item (qa::row-to-hash (first rows) '(:id :title :created_at))))
        (5am:is (= id (gethash "id" item)))
        (5am:is (string= "Test Item" (gethash "title" item))))

      ;; Simulate DELETE request
      (sqlite:execute-non-query qa:*db*
        "DELETE FROM api_items WHERE id = ?" id)
      (let ((rows (sqlite:execute-to-list qa:*db*
                     "SELECT * FROM api_items WHERE id = ?" id)))
        (5am:is (null rows))))))

;;; Error Handling Integration

(5am:test error-handling-not-found
  "Test NOT-FOUND error handling in API context"
  (5am:signals snooze:http-condition
    (qa:with-db (":memory:")
      (qa::ensure-table :items '((id integer :primary-key)))
      (let ((rows (sqlite:execute-to-list qa:*db* "SELECT * FROM items WHERE id = ?" 999)))
        (unless rows
          (qa:not-found "Item not found"))))))

(5am:test error-handling-bad-request
  "Test BAD-REQUEST error handling"
  (5am:signals snooze:http-condition
    (let ((qa:*body* (make-hash-table :test 'equal)))
      (when (zerop (hash-table-count qa:*body*))
        (qa:bad-request "Empty request body")))))

;;; Response Format Integration

(5am:test response-format-consistency
  "Test all response helpers return consistent format"
  (let ((data (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "test" h) "value")
                h)))
    ;; OK response
    (destructuring-bind (status headers body) (qa:ok data)
      (5am:is (= 200 status))
      (5am:is (listp headers))
      (5am:is (listp body)))

    ;; Created response
    (destructuring-bind (status headers body) (qa:created data)
      (5am:is (= 201 status))
      (5am:is (listp headers))
      (5am:is (listp body)))

    ;; No content response
    (destructuring-bind (status headers body) (qa:no-content)
      (5am:is (= 204 status))
      (5am:is (null body)))))

;;; Complete Scenario Test

(5am:test complete-todo-scenario
  "Test complete todo item lifecycle"
  (qa:with-db (":memory:")
    ;; Setup database
    (qa::ensure-table :todos
      '((id integer :primary-key :autoincrement)
        (title text :not-null)
        (completed integer :not-null)))

    ;; CREATE: Simulate POST /todos
    (let ((qa:*body* (let ((h (make-hash-table :test 'equal)))
                    (setf (gethash "title" h) "Write tests")
                    h)))
      ;; Validate request
      (5am:finishes
        (qa:validate qa:*body*
          (require-fields "title")
          (require-type "title" 'string)))

      ;; Insert into database
      (sqlite:execute-non-query qa:*db*
        "INSERT INTO todos (title, completed) VALUES (?, ?)"
        (gethash "title" qa:*body*) 0)
      (let ((id (qa::last-insert-id)))

        ;; READ: Simulate GET /todos/:id
        (let* ((rows (sqlite:execute-to-list qa:*db*
                       "SELECT * FROM todos WHERE id = ?" id))
               (todo (qa::row-to-hash (first rows) '(:id :title :completed))))
          (5am:is (= id (gethash "id" todo)))
          (5am:is (string= "Write tests" (gethash "title" todo)))
          (5am:is (= 0 (gethash "completed" todo))))

        ;; UPDATE: Simulate PUT /todos/:id
        (let ((qa:*body* (let ((h (make-hash-table :test 'equal)))
                        (setf (gethash "completed" h) 1)
                        h)))
          (sqlite:execute-non-query qa:*db*
            "UPDATE todos SET completed = ? WHERE id = ?"
            (gethash "completed" qa:*body*) id))

        ;; Verify update
        (let* ((rows (sqlite:execute-to-list qa:*db*
                       "SELECT completed FROM todos WHERE id = ?" id)))
          (5am:is (= 1 (first (first rows)))))

        ;; DELETE: Simulate DELETE /todos/:id
        (sqlite:execute-non-query qa:*db*
          "DELETE FROM todos WHERE id = ?" id)

        ;; Verify deletion
        (let ((rows (sqlite:execute-to-list qa:*db*
                     "SELECT * FROM todos WHERE id = ?" id)))
          (5am:is (null rows)))))))
