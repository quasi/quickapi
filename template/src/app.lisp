;;;; ABOUTME: Template application - copy and customize for your API

(defpackage :my-api
  (:use :cl :quickapi)
  (:export #:main))

(in-package :my-api)

;;; Configuration

(defvar *db-path*
  (or (uiop:getenv "DB_PATH") "app.db"))

(defvar *port*
  (parse-integer (or (uiop:getenv "PORT") "8000")))

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

;; Add your routes here:
;;
;; (GET "/items" ()
;;   (with-db (*db-path*)
;;     (sqlite:select *db* :items)))
;;
;; (POST "/items" ()
;;   (validate *body*
;;     (require-fields "name"))
;;   (with-db (*db-path*)
;;     (sqlite:insert *db* :items
;;       (list :name (gethash "name" *body*)))
;;     (created (gethash "name" *body*))))

;;; Entry Point

(defun main ()
  "Initialize and start the API."
  (init-database)
  (format t "~&Starting My API on port ~a~%" *port*)
  (start :port *port*))
