;;;; ABOUTME: Package definitions for quickapi - curated JSON API stack for Common Lisp

(defpackage :quickapi
  (:use :cl)
  (:export
   ;; Core API definition
   #:defapi
   #:start
   #:stop
   #:*debug-mode*
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

   ;; Condition types (hierarchy)
   #:quickapi-error
   #:http-error
   #:client-error
   #:server-error
   #:bad-request-error
   #:unauthorized-error
   #:forbidden-error
   #:not-found-error
   #:conflict-error
   #:validation-error
   #:internal-server-error
   #:database-error
   #:record-not-found
   #:duplicate-record
   #:connection-error
   #:authentication-error
   #:invalid-credentials

   ;; Condition accessors
   #:error-message
   #:http-error-status
   #:http-error-details
   #:validation-errors
   #:db-error-operation
   #:db-error-table
   #:duplicate-field
   #:error-cause

   ;; Condition signaling helpers
   #:bad-request
   #:unauthorized
   #:forbidden
   #:not-found
   #:conflict
   #:internal-error

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
   #:session-get

   ;; Configuration (.env support)
   #:load-env-file
   #:getenv
   #:getenv-int
   #:getenv-bool
   #:getenv-list
   #:require-env
   #:ensure-env-loaded
   #:reload-env
   #:print-env-template

   ;; Deployment utilities
   #:generate-start-script
   #:generate-systemd-unit
   #:generate-smoke-tests
   #:generate-healthcheck
   #:generate-deployment-files))

(in-package :quickapi)

;;; Global configuration

(defvar *debug-mode* nil
  "Whether the server is running in debug mode.
   When true:
   - Enables detailed error messages
   - Uses faster (less secure) password hashing for testing
   - Shows backtraces in responses")

;;; Special variables for request context

(defvar *request* nil
  "The current Lack request env. Bound during request handling.")

(defvar *body* nil
  "Parsed JSON body of the current request. Automatically parsed for
   POST, PUT, and PATCH requests with application/json content type.")

(defvar *db* nil
  "Current database connection. Bind with WITH-DB for database operations.")
