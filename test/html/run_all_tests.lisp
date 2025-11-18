#!/usr/bin/env lsp

;; Master Test Runner for HTML Library
;; Runs all unit tests for the HTML parser/renderer

(import "lib/test/unittest.inc")

(defun run-test-file (filename)
	; Run a test file and return success status
	(print "")
	(print "========================================")
	(print "Running: " filename)
	(print "========================================")
	; TODO: Load and run the test file
	; For now, just indicate the file
	:t)

(defun main ()
	(print "")
	(print "ChrysaLisp HTML Library - Test Suite")
	(print "Ported from KDE KHTML")
	(print "")

	(defq all-passed :t)

	; Run encoding tests
	(print "")
	(print "========================================")
	(print "Encoding Detector Tests")
	(print "========================================")
	(import "test/html/test_encoding.lisp")
	; The test will run when imported

	; Run part tests
	(print "")
	(print "========================================")
	(print "HTML Part Tests")
	(print "========================================")
	(import "test/html/test_part.lisp")

	; Run parser tests
	(print "")
	(print "========================================")
	(print "HTML Parser Tests")
	(print "========================================")
	(import "test/html/test_parser.lisp")

	(print "")
	(print "========================================")
	(print "All Tests Complete")
	(print "========================================")
	(print ""))
