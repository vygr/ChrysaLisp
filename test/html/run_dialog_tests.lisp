#!/usr/bin/env lsp

;; Dialog Function Test Runner
;; Runs all dialog function tests (alert/confirm/prompt)

(import "lib/test/unittest.inc")

(defun main ()
	(print "")
	(print "ChrysaLisp HTML Script Library - Dialog Function Tests")
	(print "")

	; Run dialog tests
	(print "")
	(print "========================================")
	(print "Dialog Function Tests")
	(print "========================================")
	(import "test/html/test_dialogs.lisp")

	(print "")
	(print "========================================")
	(print "All Dialog Tests Complete")
	(print "========================================")
	(print ""))

(main)
