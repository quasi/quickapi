;;;; ABOUTME: Full CRUD example with SQLite for quickapi

;;; A complete todo API demonstrating:
;;; - All CRUD operations
;;; - SQLite database integration
;;; - Request validation
;;; - Error handling

(defpackage :todo-api
  (:use :cl :quickapi))

(in-package :todo-api)

;;; Database setup

(defvar *db-path* "todos.db")

(defun init-database ()
  "Initialize the todos database."
  (with-db (*db-path*)
    (ensure-table :todos
      '((id integer :primary-key :autoincrement)
        (title text :not-null)
        (description text)
        (completed integer :not-null)
        (created_at text :not-null)))))

;;; Helper functions

(defun make-timestamp ()
  "Get current timestamp as ISO string."
  (multiple-value-bind (sec min hour day month year)
      (get-decoded-time)
    (format nil "~4,'0d-~2,'0d-~2,'0dT~2,'0d:~2,'0d:~2,'0d"
            year month day hour min sec)))

(defun todo-to-hash (row)
  "Convert a todo database row to a hash-table."
  (destructuring-bind (id title description completed created-at) row
    (let ((h (make-hash-table :test 'equal)))
      (setf (gethash "id" h) id)
      (setf (gethash "title" h) title)
      (setf (gethash "description" h) description)
      (setf (gethash "completed" h) (= completed 1))
      (setf (gethash "created_at" h) created-at)
      h)))

;;; API Definition

(defapi todo-api
  :name "Todo API"
  :version "1.0"
  :description "Simple todo list API built with quickapi")

;;; Routes

;; List all todos
(api-get "/todos" ()
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos :order-by '(:created_at :desc))))
      (mapcar #'todo-to-hash rows))))

;; Get a single todo by ID
(api-get "/todos/:id" (id)
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (if rows
          (todo-to-hash (first rows))
          (not-found "Todo not found")))))

;; Create a new todo
(api-post "/todos" ()
  (validate *body*
    (require-fields "title")
    (require-type "title" 'string)
    (require-length "title" :min 1 :max 200))

  (with-db (*db-path*)
    (sqlite:insert *db* :todos
      (list :title (gethash "title" *body*)
            :description (gethash "description" *body* "")
            :completed 0
            :created_at (make-timestamp)))
    (let ((id (last-insert-id)))
      (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
        (created (todo-to-hash (first rows)))))))

;; Update a todo
(api-put "/todos/:id" (id)
  (validate *body*
    (require-type "title" 'string)
    (require-type "completed" 'boolean))

  (with-db (*db-path*)
    ;; Check if todo exists
    (unless (sqlite:select *db* :todos :where `(:= :id ,id))
      (not-found "Todo not found"))

    ;; Build update data
    (let ((update-data '()))
      (when (gethash "title" *body*)
        (push (gethash "title" *body*) update-data)
        (push :title update-data))
      (when (gethash "description" *body*)
        (push (gethash "description" *body*) update-data)
        (push :description update-data))
      (when (gethash "completed" *body*)
        (push (if (gethash "completed" *body*) 1 0) update-data)
        (push :completed update-data))

      (when update-data
        (sqlite:update-table *db* :todos update-data
          :where `(:= :id ,id))))

    ;; Return updated todo
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (todo-to-hash (first rows)))))

;; Delete a todo
(api-delete "/todos/:id" (id)
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (unless rows
        (not-found "Todo not found"))
      (sqlite:delete-from *db* :todos :where `(:= :id ,id))
      (no-content))))

;; Toggle todo completion status
(api-post "/todos/:id/toggle" (id)
  (with-db (*db-path*)
    (let ((rows (sqlite:select *db* :todos :where `(:= :id ,id))))
      (unless rows
        (not-found "Todo not found"))
      (let* ((todo (first rows))
             (current (nth 3 todo))
             (new-value (if (= current 1) 0 1)))
        (sqlite:update-table *db* :todos
          (list :completed new-value)
          :where `(:= :id ,id))
        (let ((updated (sqlite:select *db* :todos :where `(:= :id ,id))))
          (todo-to-hash (first updated)))))))

;;; Startup

(defun main ()
  "Initialize database and start the server."
  (init-database)
  (start :port 8000))

;;; Usage:
;;;
;;; Initialize and start:
;;;   (todo-api:main)
;;;
;;; Or manually:
;;;   (todo-api:init-database)
;;;   (quickapi:start :port 8000)
;;;
;;; Test with curl:
;;;
;;;   # List todos
;;;   curl http://localhost:8000/todos
;;;
;;;   # Create todo
;;;   curl -X POST -H "Content-Type: application/json" \
;;;        -d '{"title":"Buy milk"}' \
;;;        http://localhost:8000/todos
;;;
;;;   # Get single todo
;;;   curl http://localhost:8000/todos/1
;;;
;;;   # Update todo
;;;   curl -X PUT -H "Content-Type: application/json" \
;;;        -d '{"completed":true}' \
;;;        http://localhost:8000/todos/1
;;;
;;;   # Toggle completion
;;;   curl -X POST http://localhost:8000/todos/1/toggle
;;;
;;;   # Delete todo
;;;   curl -X DELETE http://localhost:8000/todos/1
