;;;; ABOUTME: Tests for validation functions and VALIDATE macro

(in-package :quickapi/tests)

(5am:in-suite quickapi-tests)

;;; Helper to create test data
(defun make-test-data (&rest pairs)
  "Create hash-table from key-value pairs."
  (let ((h (make-hash-table :test 'equal)))
    (loop for (key value) on pairs by #'cddr
          do (setf (gethash key h) value))
    h))

;;; REQUIRE-FIELDS Tests

(5am:test require-fields-all-present
  "Test require-fields passes when all fields present"
  (let ((data (make-test-data "name" "Alice" "email" "alice@example.com")))
    (5am:is (null (let ((qa::*validation-errors* nil))
                (qa::require-fields data "name" "email")
                qa::*validation-errors*)))))

(5am:test require-fields-missing-field
  "Test require-fields detects missing field"
  (let ((data (make-test-data "name" "Alice")))
    (let ((qa::*validation-errors* nil))
      (qa::require-fields data "name" "email")
      (5am:is (= 1 (length qa::*validation-errors*))))))

(5am:test require-fields-empty-string
  "Test require-fields treats empty string as missing"
  (let ((data (make-test-data "name" "")))
    (let ((qa::*validation-errors* nil))
      (qa::require-fields data "name")
      (5am:is (= 1 (length qa::*validation-errors*))))))

;;; REQUIRE-TYPE Tests

(5am:test require-type-string-valid
  "Test require-type accepts valid string"
  (let ((data (make-test-data "name" "Alice")))
    (let ((qa::*validation-errors* nil))
      (qa::require-type "name" 'string data)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-type-string-invalid
  "Test require-type rejects non-string"
  (let ((data (make-test-data "name" 123)))
    (let ((qa::*validation-errors* nil))
      (qa::require-type "name" 'string data)
      (5am:is (= 1 (length qa::*validation-errors*))))))

(5am:test require-type-number-valid
  "Test require-type accepts valid number"
  (let ((data (make-test-data "age" 25)))
    (let ((qa::*validation-errors* nil))
      (qa::require-type "age" 'number data)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-type-integer-valid
  "Test require-type accepts valid integer"
  (let ((data (make-test-data "count" 42)))
    (let ((qa::*validation-errors* nil))
      (qa::require-type "count" 'integer data)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-type-boolean-valid
  "Test require-type accepts valid boolean"
  (let ((data (make-test-data "active" t)))
    (let ((qa::*validation-errors* nil))
      (qa::require-type "active" 'boolean data)
      (5am:is (null qa::*validation-errors*)))))

;;; REQUIRE-LENGTH Tests

(5am:test require-length-min-valid
  "Test require-length accepts string meeting minimum"
  (let ((data (make-test-data "name" "Alice")))
    (let ((qa::*validation-errors* nil))
      (qa::require-length "name" data :min 3)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-length-min-invalid
  "Test require-length rejects string below minimum"
  (let ((data (make-test-data "name" "Al")))
    (let ((qa::*validation-errors* nil))
      (qa::require-length "name" data :min 3)
      (5am:is (= 1 (length qa::*validation-errors*))))))

(5am:test require-length-max-valid
  "Test require-length accepts string meeting maximum"
  (let ((data (make-test-data "name" "Alice")))
    (let ((qa::*validation-errors* nil))
      (qa::require-length "name" data :max 10)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-length-max-invalid
  "Test require-length rejects string exceeding maximum"
  (let ((data (make-test-data "name" "Verylongname")))
    (let ((qa::*validation-errors* nil))
      (qa::require-length "name" data :max 10)
      (5am:is (= 1 (length qa::*validation-errors*))))))

(5am:test require-length-range-valid
  "Test require-length with both min and max"
  (let ((data (make-test-data "name" "Alice")))
    (let ((qa::*validation-errors* nil))
      (qa::require-length "name" data :min 3 :max 10)
      (5am:is (null qa::*validation-errors*)))))

;;; REQUIRE-RANGE Tests

(5am:test require-range-min-valid
  "Test require-range accepts number meeting minimum"
  (let ((data (make-test-data "age" 25)))
    (let ((qa::*validation-errors* nil))
      (qa::require-range "age" data :min 18)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-range-min-invalid
  "Test require-range rejects number below minimum"
  (let ((data (make-test-data "age" 15)))
    (let ((qa::*validation-errors* nil))
      (qa::require-range "age" data :min 18)
      (5am:is (= 1 (length qa::*validation-errors*))))))

(5am:test require-range-max-valid
  "Test require-range accepts number meeting maximum"
  (let ((data (make-test-data "score" 95)))
    (let ((qa::*validation-errors* nil))
      (qa::require-range "score" data :max 100)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-range-max-invalid
  "Test require-range rejects number exceeding maximum"
  (let ((data (make-test-data "score" 105)))
    (let ((qa::*validation-errors* nil))
      (qa::require-range "score" data :max 100)
      (5am:is (= 1 (length qa::*validation-errors*))))))

;;; REQUIRE-PATTERN Tests

(5am:test require-pattern-valid
  "Test require-pattern accepts matching string"
  (let ((data (make-test-data "email" "alice@example.com")))
    (let ((qa::*validation-errors* nil))
      (qa::require-pattern "email" "^[^@]+@[^@]+\\.[^@]+$" data)
      (5am:is (null qa::*validation-errors*)))))

(5am:test require-pattern-invalid
  "Test require-pattern rejects non-matching string"
  (let ((data (make-test-data "email" "not-an-email")))
    (let ((qa::*validation-errors* nil))
      (qa::require-pattern "email" "^[^@]+@[^@]+\\.[^@]+$" data)
      (5am:is (= 1 (length qa::*validation-errors*))))))

;;; VALIDATE Macro Integration Tests

(5am:test validate-macro-all-pass
  "Test VALIDATE macro with all checks passing"
  (let ((data (make-test-data "name" "Alice" "age" 25)))
    (5am:finishes
      (qa:validate data
        (require-fields "name" "age")
        (require-type "name" 'string)
        (require-type "age" 'number)))))

(5am:test validate-macro-signal-error
  "Test VALIDATE macro signals condition on validation failure"
  (let ((data (make-test-data "name" "")))
    (5am:signals snooze:http-condition
      (qa:validate data
        (require-fields "name" "email")))))

(5am:test validate-macro-multiple-errors
  "Test VALIDATE macro collects multiple errors"
  (let ((data (make-test-data "name" "" "age" -5)))
    (handler-case
        (qa:validate data
          (require-fields "name" "email")
          (require-range "age" :min 0))
      (snooze:http-condition ()
        ;; Expected - multiple validation errors should be collected
        (5am:pass))
      (:no-error ()
        (5am:fail "Expected validation to signal condition")))))
