;;;; ABOUTME: Minimal hello-world example for quickapi

;;; Load quickapi (assumes you've set up ASDF to find it)
;;; (ql:quickload :quickapi)

(defpackage :hello-world
  (:use :cl :quickapi))

(in-package :hello-world)

;;; Define a simple API
(defapi hello-api
  :name "Hello World API"
  :version "1.0")

;;; Simple GET endpoint returning JSON
(api-get "/" ()
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "message" response) "Hello, World!")
    (setf (gethash "version" response) "1.0")
    response))

;;; GET with path parameter
(api-get "/greet/:name" (name)
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "message" response) (format nil "Hello, ~a!" name))
    response))

;;; POST endpoint that echoes the body
(api-post "/echo" ()
  (let ((response (make-hash-table :test 'equal)))
    (setf (gethash "you_sent" response) *body*)
    response))

;;; Start the server
;;; (start :port 8000)

;;; Test with:
;;;   curl http://localhost:8000/
;;;   curl http://localhost:8000/greet/Alice
;;;   curl -X POST -H "Content-Type: application/json" -d '{"msg":"test"}' http://localhost:8000/echo
