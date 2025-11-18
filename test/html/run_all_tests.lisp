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

	; Run DOM assertion tests
	(print "")
	(print "========================================")
	(print "DOM Assertion Tests")
	(print "========================================")
	(import "test/html/test_dom_assertions.lisp")

	; Run round-trip serialization tests
	(print "")
	(print "========================================")
	(print "HTML Round-trip Serialization Tests")
	(print "========================================")
	(import "test/html/test_html_roundtrip.lisp")

	; Run canvas rendering tests
	(print "")
	(print "========================================")
	(print "Canvas Rendering Tests")
	(print "========================================")
	(import "test/html/test_canvas_rendering.lisp")

	; Run interactive hyperlink tests
	(print "")
	(print "========================================")
	(print "Interactive Hyperlink Tests")
	(print "========================================")
	(import "test/html/test_interactive_links.lisp")

	; Run text selection tests
	(print "")
	(print "========================================")
	(print "Text Selection Tests")
	(print "========================================")
	(import "test/html/test_text_selection.lisp")

	; Run script execution tests
	(print "")
	(print "========================================")
	(print "Script Execution Tests")
	(print "========================================")
	(import "test/html/test_script_execution.lisp")

	; Run browser navigation tests
	(print "")
	(print "========================================")
	(print "Browser Navigation Tests")
	(print "========================================")
	(import "test/html/test_browser_navigation.lisp")

	; Run devtools inspector tests
	(print "")
	(print "========================================")
	(print "DevTools Inspector Tests")
	(print "========================================")
	(import "test/html/test_devtools.lisp")

	; Run JSON roundtrip tests
	(print "")
	(print "========================================")
	(print "JSON Roundtrip Tests")
	(print "========================================")
	(import "test/json/test_json_roundtrip.lisp")

	(print "")
	(print "========================================")
	(print "All Tests Complete")
	(print "========================================")
	(print ""))
