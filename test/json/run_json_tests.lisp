#!/usr/bin/env lsp

;; JSON Test Runner
;; Runs all JSON roundtrip tests

(import "lib/test/unittest.inc")

(defun main ()
	(print "")
	(print "ChrysaLisp JSON Library - Test Suite")
	(print "XStream-style Roundtrip Tests")
	(print "")

	; Run roundtrip tests
	(print "")
	(print "========================================")
	(print "JSON Roundtrip Tests")
	(print "========================================")
	(import "test/json/test_json_roundtrip.lisp")

	(print "")
	(print "========================================")
	(print "All JSON Tests Complete")
	(print "========================================")
	(print ""))

(main)
