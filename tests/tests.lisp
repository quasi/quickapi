;;;; ABOUTME: Main test file that loads all test suites

(in-package :quickapi/tests)

;;; This file ensures all test files are loaded in the correct order
;;; Individual test files are:
;;; - response-tests.lisp
;;; - validation-tests.lisp
;;; - sqlite-tests.lisp
;;; - integration-tests.lisp

;;; The tests are run via (asdf:test-system :quickapi)
;;; or directly with (quickapi/tests:run-tests)
