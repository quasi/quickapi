;;;; ABOUTME: Validation functions for quickapi - simple validation without DSL

(in-package :quickapi)

;;; Validation Condition

(define-condition validation-error (error)
  ((errors :initarg :errors
           :reader validation-errors
           :documentation "List of validation error details"))
  (:report (lambda (c stream)
             (format stream "Validation failed: ~a error(s)"
                     (length (validation-errors c)))))
  (:documentation "Signalled when request validation fails."))

;;; Validation Error Collection

(defvar *validation-errors* nil
  "Collects validation errors during a VALIDATE block.")

(defun add-validation-error (field message)
  "Add a validation error for FIELD with MESSAGE to the current collection."
  (let ((error (make-hash-table :test 'equal)))
    (setf (gethash "field" error) field)
    (setf (gethash "message" error) message)
    (push error *validation-errors*)))

;;; Main Validation Macro

(defmacro validate (data &body checks)
  "Validate DATA using the provided CHECKS. Each check is a function call
   that may add errors to the validation context. If any errors are found,
   signals a validation-error condition which is handled by returning 422.

   Example:
     (validate *body*
       (require-fields \"name\" \"email\")
       (require-type \"age\" 'number)
       (require-range \"age\" :min 0 :max 150))"
  (let ((data-var (gensym "DATA")))
    `(let ((*validation-errors* nil)
           (,data-var ,data))
       (declare (ignorable ,data-var))
       ,@(mapcar (lambda (check)
                   `(,@check ,data-var))
                 checks)
       (when *validation-errors*
         (signal-validation-error)))))

(defun signal-validation-error ()
  "Signal the collected validation errors as a 422 response."
  (snooze:http-condition 422
    (com.inuoe.jzon:stringify
     (let ((response (make-hash-table :test 'equal)))
       (setf (gethash "error" response) "validation_error")
       (setf (gethash "message" response) "Validation failed")
       (setf (gethash "details" response) (nreverse *validation-errors*))
       response))))

;;; Validation Check Functions
;;; Each takes the data hash-table as its last argument

(defun require-fields (data &rest fields)
  "Check that all FIELDS are present and non-nil in DATA."
  (dolist (field fields)
    (let ((value (gethash field data)))
      (when (or (null value)
                (and (stringp value) (string= value "")))
        (add-validation-error field "required")))))

(defun require-type (field expected-type data)
  "Check that FIELD in DATA has the EXPECTED-TYPE.
   Supported types: string, number, integer, boolean, list, hash-table."
  (let ((value (gethash field data)))
    (when value
      (unless (case expected-type
                (string (stringp value))
                (number (numberp value))
                (integer (integerp value))
                (boolean (or (eq value t) (eq value nil)
                             (eq value :true) (eq value :false)))
                (list (listp value))
                (hash-table (hash-table-p value))
                (t t))
        (add-validation-error field
          (format nil "must be ~(~a~)" expected-type))))))

(defun require-length (field data &key min max)
  "Check that the length of FIELD in DATA is within bounds.
   Works for strings, lists, and vectors."
  (let ((value (gethash field data)))
    (when value
      (let ((len (typecase value
                   (string (length value))
                   (list (length value))
                   (vector (length value))
                   (t nil))))
        (when len
          (when (and min (< len min))
            (add-validation-error field
              (format nil "must be at least ~a characters" min)))
          (when (and max (> len max))
            (add-validation-error field
              (format nil "must be at most ~a characters" max))))))))

(defun require-range (field data &key min max)
  "Check that the numeric value of FIELD in DATA is within bounds."
  (let ((value (gethash field data)))
    (when (and value (numberp value))
      (when (and min (< value min))
        (add-validation-error field
          (format nil "must be at least ~a" min)))
      (when (and max (> value max))
        (add-validation-error field
          (format nil "must be at most ~a" max))))))

(defun require-pattern (field pattern data)
  "Check that FIELD in DATA matches the regex PATTERN."
  (let ((value (gethash field data)))
    (when (and value (stringp value))
      (unless (cl-ppcre:scan pattern value)
        (add-validation-error field "invalid format")))))

;;; Convenience: get validated field value

(defun field (data name &optional default)
  "Get field NAME from DATA hash-table, or DEFAULT if not present."
  (gethash name data default))
