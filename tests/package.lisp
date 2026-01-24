;;;; ABOUTME: Test package definitions for quickapi

(defpackage :quickapi/tests
  (:use :cl :quickapi)
  (:local-nicknames (:5am :fiveam)
                    (:qa :quickapi))
  (:export #:quickapi-tests
           #:run-tests))

(in-package :quickapi/tests)

(5am:def-suite quickapi-tests
  :description "Test suite for quickapi")

(defun run-tests ()
  "Run all quickapi tests."
  (5am:run! 'quickapi-tests))
