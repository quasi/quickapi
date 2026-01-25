;;;; ABOUTME: Tests for response helpers (ok, created, not-found, etc.)

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; Helper to parse JSON response body
(defun parse-response-body (response)
  "Extract and parse JSON from response triple."
  (destructuring-bind (status headers body-list) response
    (declare (ignore headers))
    (values (com.inuoe.jzon:parse (first body-list))
            status)))

;;; Success Response Tests

(5am:test ok-response
  "Test OK helper returns 200 with JSON data"
  (let ((data (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "message" h) "success")
                h)))
    (multiple-value-bind (parsed-body status)
        (parse-response-body (qa:ok data))
      (5am:is (= 200 status))
      (5am:is (string= "success" (gethash "message" parsed-body))))))

(5am:test created-response
  "Test CREATED helper returns 201"
  (let ((data (let ((h (make-hash-table :test 'equal)))
                (setf (gethash "id" h) 123)
                h)))
    (multiple-value-bind (parsed-body status)
        (parse-response-body (qa:created data))
      (5am:is (= 201 status))
      (5am:is (= 123 (gethash "id" parsed-body))))))

(5am:test no-content-response
  "Test NO-CONTENT helper returns 204 with empty body"
  (destructuring-bind (status headers body-list) (qa:no-content)
    (5am:is (= 204 status))
    (5am:is (null body-list))))

;;; Error Response Tests

(5am:test bad-request-signals-condition
  "Test BAD-REQUEST signals HTTP condition with 400"
  (5am:signals qa:http-error
    (qa:bad-request "Invalid input")))

(5am:test not-found-signals-condition
  "Test NOT-FOUND signals HTTP condition with 404"
  (5am:signals qa:http-error
    (qa:not-found "Resource not found")))

(5am:test error-response-custom-status
  "Test ERROR-RESPONSE with custom status code"
  (5am:signals qa:http-error
    (qa:error-response 409 "Conflict detected")))

;;; Format Error Tests

(5am:test format-error-basic
  "Test format-error creates proper structure"
  (let ((result (qa::format-error "test_error" "Test message")))
    (5am:is (string= "test_error" (gethash "error" result)))
    (5am:is (string= "Test message" (gethash "message" result)))
    (5am:is (null (gethash "details" result)))))

(5am:test format-error-with-details
  "Test format-error includes details when provided"
  (let* ((details (list (let ((h (make-hash-table :test 'equal)))
                          (setf (gethash "field" h) "email")
                          h)))
         (result (qa::format-error "validation_error" "Failed" details)))
    (5am:is (string= "validation_error" (gethash "error" result)))
    (5am:is (equal details (gethash "details" result)))))

(5am:test status-to-error-type
  "Test status code to error type conversion"
  (5am:is (string= "bad_request" (qa::status-to-error-type 400)))
  (5am:is (string= "unauthorized" (qa::status-to-error-type 401)))
  (5am:is (string= "not_found" (qa::status-to-error-type 404)))
  (5am:is (string= "validation_error" (qa::status-to-error-type 422)))
  (5am:is (string= "internal_error" (qa::status-to-error-type 500))))
