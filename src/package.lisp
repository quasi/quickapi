;;;; ABOUTME: Package definitions for quickapi - curated JSON API stack for Common Lisp

(defpackage :quickapi
  (:use :cl)
  (:export
   ;; Core API definition
   #:defapi
   #:start
   #:stop
   #:api-name
   #:api-version
   #:api-description

   ;; Route macros
   #:api-get
   #:api-post
   #:api-put
   #:api-delete
   #:api-patch

   ;; Request context
   #:*request*
   #:*body*
   #:body

   ;; Response helpers
   #:ok
   #:created
   #:no-content
   #:bad-request
   #:not-found
   #:error-response
   #:json-response

   ;; Validation
   #:validate
   #:require-fields
   #:require-type
   #:require-length
   #:require-range
   #:require-pattern
   #:validation-error
   #:validation-errors

   ;; Database (re-exports from sqlite)
   #:*db*
   #:with-db
   #:ensure-table
   #:last-insert-id
   #:row-to-hash
   #:rows-to-json
   #:with-transaction

   ;; Models
   #:defmodel
   #:migrate-models
   #:*models*

   ;; HTTP conditions
   #:http-error
   #:http-condition

   ;; Authentication
   #:*current-user*
   #:*jwt-secret*
   #:*jwt-algorithm*
   #:*session-user-loader*
   #:*api-key-validator*
   #:generate-jwt
   #:verify-jwt
   #:hash-password
   #:verify-password
   #:session-get))

(in-package :quickapi)

;;; Special variables for request context

(defvar *request* nil
  "The current Lack request env. Bound during request handling.")

(defvar *body* nil
  "Parsed JSON body of the current request. Automatically parsed for
   POST, PUT, and PATCH requests with application/json content type.")

(defvar *db* nil
  "Current database connection. Bind with WITH-DB for database operations.")
