;;;; ABOUTME: Template application - copy and customize for your API

(defpackage :my-api
  (:use :cl :quickapi)
  (:export #:main))

(in-package :my-api)

;;; Configuration

(defvar *db-path*
  (or (uiop:getenv "DB_PATH") "app.db"))

(defvar *default-port* 8000)

;;; CLI argument parsing

(defun parse-cli-args ()
  "Parse command-line arguments. Returns plist of options.
   Supports: --port NUM"
  (let ((args (uiop:command-line-arguments))
        (result nil))
    (loop while args do
      (let ((arg (pop args)))
        (cond
          ((string= arg "--port")
           (let ((val (pop args)))
             (unless val
               (format *error-output* "~&Error: --port requires a number~%")
               (uiop:quit 1))
             (let ((port (parse-integer val :junk-allowed t)))
               (unless (and port (< 0 port 65536))
                 (format *error-output* "~&Error: invalid port number: ~a~%" val)
                 (uiop:quit 1))
               (setf (getf result :port) port))))
          (t
           (format *error-output* "~&Unknown argument: ~a~%" arg)
           (format *error-output* "Usage: my-api [--port NUM]~%")
           (uiop:quit 1)))))
    result))

;;; Database Setup

(defun init-database ()
  "Initialize database tables."
  (with-db (*db-path*)
    ;; Add your tables here
    ;; (ensure-table :users
    ;;   '((id integer :primary-key :autoincrement)
    ;;     (name text :not-null)))
    ))

;;; API Definition

(defapi my-api
  :name "My API"
  :version "1.0")

;;; Routes

(api-get "/" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "message" h) "Welcome to My API")
    (setf (gethash "version" h) "1.0")
    h))

(api-get "/health" ()
  (let ((h (make-hash-table :test 'equal)))
    (setf (gethash "status" h) "ok")
    h))

(api-get "/echo" ()
  "Echo back query parameters as JSON."
  (let ((query-string (getf *request* :query-string))
        (result (make-hash-table :test 'equal)))
    (when query-string
      (dolist (pair (uiop:split-string query-string :separator "&"))
        (let ((kv (uiop:split-string pair :separator "=")))
          (when (= (length kv) 2)
            (setf (gethash (first kv) result) (second kv))))))
    result))

;;; Entry Point

(defun main ()
  "Initialize and start the API."
  (let* ((opts (parse-cli-args))
         (port (or (getf opts :port)
                   (let ((env (uiop:getenv "PORT")))
                     (when env (parse-integer env :junk-allowed t)))
                   *default-port*)))
    (init-database)
    (format t "~&Starting My API on http://127.0.0.1:~a/~%" port)
    (start :port port :address "127.0.0.1")
    ;; Keep the process alive
    (handler-case
        (loop (sleep 3600))
      (#+sbcl sb-sys:interactive-interrupt
       #+ccl ccl:interrupt-signal-condition
       #-(or sbcl ccl) condition
       ()
        (format t "~&Shutting down...~%")
        (stop)
        (uiop:quit 0)))))
