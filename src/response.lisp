;;;; ABOUTME: Response helpers for quickapi - semantic core for HTTP responses

(in-package :quickapi)

;;; JSON Response Construction

(defun json-response (data &key (status 200))
  "Create a JSON response with the given data and HTTP status code.
   DATA can be a hash-table, plist, alist, list, or any jzon-serializable value."
  (list status
        '(:content-type "application/json; charset=utf-8")
        (list (com.inuoe.jzon:stringify data))))

;;; Success Response Helpers

(defun make-response-data (args)
  "Convert response arguments to JSON-serializable data.
   If ARGS is a single value, return it as-is.
   If ARGS is keyword pairs, build a hash-table from them."
  (cond
    ;; No args - empty object
    ((null args)
     (make-hash-table :test 'equal))
    ;; Single non-keyword argument - return as-is
    ((and (= 1 (length args))
          (not (keywordp (first args))))
     (first args))
    ;; Keyword pairs - build hash-table
    ((keywordp (first args))
     (let ((ht (make-hash-table :test 'equal)))
       (loop for (key val) on args by #'cddr
             do (setf (gethash (string-downcase (symbol-name key)) ht) val))
       ht))
    ;; Single argument
    (t (first args))))

(defun ok (&rest args)
  "Return 200 OK with JSON data.
   Can be called as:
     (ok hash-table)           ; Pass existing data
     (ok :key1 val1 :key2 val2) ; Build object from keyword pairs"
  (json-response (make-response-data args) :status 200))

(defun created (&rest args)
  "Return 201 Created with JSON data.
   Can be called as:
     (created hash-table)           ; Pass existing data
     (created :key1 val1 :key2 val2) ; Build object from keyword pairs"
  (json-response (make-response-data args) :status 201))

(defun no-content ()
  "Return 204 No Content with empty body."
  (list 204 '() '()))

;;; Error Response Helpers
;;; Note: Error signaling functions (bad-request, not-found, etc.) are now in conditions.lisp
;;; This file only contains success response helpers
