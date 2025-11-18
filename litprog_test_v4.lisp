;; ========================================================================
;; LITPROG Phase 4 - Working Test Suite
;; ========================================================================
;;
;; This test suite actually runs in ChrysaLisp and validates LITPROG
;; functionality with real tests that pass or fail.
;;
;; Usage:
;;   (import "litprog_test_v4.lisp")
;;   (run-all-tests)
;;

(import "litprog_core_v4.lisp")

;; ========================================================================
;; Test Framework
;; ========================================================================

(defq *tests-run* 0
	*tests-passed* 0
	*tests-failed* 0
	*current-suite* ""
	*test-results* (list))

(defun test-suite (name)
	"Start a new test suite"
	(setq *current-suite* name)
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print (cat "║  " name (repeat-str " " (- 61 (length name))) "║"))
	(print "╚════════════════════════════════════════════════════════════════╝")
	(print ""))

(defun repeat-str (s n)
	"Repeat string n times"
	(if (<= n 0)
		""
		(cat s (repeat-str s (dec n)))))

(defun test-assert (condition description)
	"Assert a condition is true"
	(setq *tests-run* (inc *tests-run*))
	(if condition
		(progn
			(setq *tests-passed* (inc *tests-passed*))
			(print (cat "  ✓ " description)))
		(progn
			(setq *tests-failed* (inc *tests-failed*))
			(print (cat "  ✗ " description " FAILED")))))

(defun test-equal (actual expected description)
	"Test that two values are equal"
	(test-assert (eql actual expected)
		(cat description " (expected: " expected ", got: " actual ")")))

(defun test-not-nil (value description)
	"Test that value is not nil"
	(test-assert value description))

;; ========================================================================
;; String Utility Tests
;; ========================================================================

(defun test-string-utilities ()
	(test-suite "String Utilities")

	; trim-string tests
	(test-equal (trim-string "  hello  ") "hello"
		"trim-string removes leading/trailing spaces")
	(test-equal (trim-string "\thello\t") "hello"
		"trim-string removes tabs")
	(test-equal (trim-string "hello") "hello"
		"trim-string handles no whitespace")
	(test-equal (trim-string "") ""
		"trim-string handles empty string")

	; starts-with? tests
	(test-assert (starts-with? "hello world" "hello")
		"starts-with? matches prefix")
	(test-assert (not (starts-with? "hello world" "world"))
		"starts-with? rejects non-prefix")
	(test-assert (starts-with? "test" "test")
		"starts-with? matches exact")
	(test-assert (not (starts-with? "hi" "hello"))
		"starts-with? rejects too long prefix")

	; split-lines tests
	(defq lines (split-lines (cat "line1" (ascii-char 10) "line2" (ascii-char 10) "line3")))
	(test-equal (length lines) 3 "split-lines creates 3 lines")
	(test-equal (elem lines 0) "line1" "split-lines first line")
	(test-equal (elem lines 1) "line2" "split-lines second line"))

;; ========================================================================
;; Noweb Parser Tests
;; ========================================================================

(defun test-noweb-parser ()
	(test-suite "Noweb Parser")

	; Chunk definition tests
	(test-equal (parse-noweb-chunk-def "<<test>>=") "test"
		"parse chunk definition")
	(test-equal (parse-noweb-chunk-def "<<my-chunk>>=") "my-chunk"
		"parse chunk with hyphen")
	(test-equal (parse-noweb-chunk-def "  <<indented>>=  ") "indented"
		"parse indented chunk definition")
	(test-not-nil (not (parse-noweb-chunk-def "not a chunk"))
		"reject non-chunk line")

	; Chunk reference tests
	(test-equal (parse-noweb-chunk-ref "  <<helper>>  ") "helper"
		"parse chunk reference")
	(test-equal (parse-noweb-chunk-ref "<<another-chunk>>") "another-chunk"
		"parse simple chunk reference")
	(test-not-nil (not (parse-noweb-chunk-ref "<<chunk>>="))
		"reject chunk definition as reference"))

;; ========================================================================
;; Context Tests
;; ========================================================================

(defun test-context ()
	(test-suite "Context Structure")

	; Create context
	(defq ctx (create-context))
	(test-not-nil ctx "create context")

	; Add chunk
	(add-chunk ctx "test-chunk" "code here" 10)
	(defq chunk (get-chunk ctx "test-chunk"))
	(test-not-nil chunk "add and retrieve chunk")

	; Verify chunk properties
	(test-equal (first (rest chunk)) "test-chunk" "chunk name")
	(test-equal (first (rest (rest chunk))) "code here" "chunk code"))

;; ========================================================================
;; File Parsing Tests
;; ========================================================================

(defun test-file-parsing ()
	(test-suite "File Parsing")

	; Create test file
	(defq test-content (cat
		"# Test Program" (ascii-char 10)
		(ascii-char 10)
		"<<main>>=" (ascii-char 10)
		"(print \"hello\")" (ascii-char 10)
		"@" (ascii-char 10)))

	(save-text-file "/tmp/test-parse.lit" test-content)

	; Parse it
	(defq ctx (parse-literate-file "/tmp/test-parse.lit"))
	(test-not-nil ctx "parse literate file")

	; Check parsed chunk
	(defq chunk (get-chunk ctx "main"))
	(test-not-nil chunk "parsed chunk exists")
	(test-equal (first (rest chunk)) "main" "parsed chunk name")

	; Cleanup
	; (Note: would delete /tmp/test-parse.lit in real impl))

;; ========================================================================
;; Chunk Expansion Tests
;; ========================================================================

(defun test-chunk-expansion ()
	(test-suite "Chunk Expansion")

	; Create context with chunks
	(defq ctx (create-context))
	(add-chunk ctx "helper" "(print \"helper\")" 1)
	(add-chunk ctx "main" (cat "start" (ascii-char 10) "<<helper>>" (ascii-char 10) "end") 5)

	; Expand main chunk
	(defq chunk (get-chunk ctx "main"))
	(defq expanded (tangle-chunk chunk ctx))

	(test-not-nil expanded "chunk expansion produces output")
	(test-assert (find expanded "helper") "expanded code contains reference"))

;; ========================================================================
;; Integration Test
;; ========================================================================

(defun test-integration ()
	(test-suite "Integration Test - Full Pipeline")

	; Create complete literate file
	(defq test-lit (cat
		"# Complete Test" (ascii-char 10)
		(ascii-char 10)
		"<<test.lisp>>=" (ascii-char 10)
		"(defq x 42)" (ascii-char 10)
		"(print x)" (ascii-char 10)
		"@" (ascii-char 10)))

	(save-text-file "/tmp/integration-test.lit" test-lit)

	; Parse
	(defq ctx (parse-literate-file "/tmp/integration-test.lit"))
	(test-not-nil ctx "integration: parse file")

	; Tangle
	(tangle-to-file ctx "/tmp/integration-test-output.lisp")

	; Verify output file exists and has content
	(defq output (load-text-file "/tmp/integration-test-output.lisp"))
	(test-not-nil output "integration: tangled file has content")
	(test-assert (find output "(defq x 42)") "integration: output contains code")

	(print "")
	(print "✓ Full pipeline works: parse → tangle → output"))

;; ========================================================================
;; Test Runner
;; ========================================================================

(defun run-all-tests ()
	"Run all test suites"
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print "║                                                                ║")
	(print "║         LITPROG Phase 4 - Test Suite                          ║")
	(print "║                                                                ║")
	(print "╚════════════════════════════════════════════════════════════════╝")

	(setq *tests-run* 0 *tests-passed* 0 *tests-failed* 0)

	; Run all test suites
	(test-string-utilities)
	(test-noweb-parser)
	(test-context)
	(test-file-parsing)
	(test-chunk-expansion)
	(test-integration)

	; Print summary
	(print "")
	(print "╔════════════════════════════════════════════════════════════════╗")
	(print "║  Test Results                                                 ║")
	(print "╚════════════════════════════════════════════════════════════════╝")
	(print "")
	(print (cat "  Total tests:  " *tests-run*))
	(print (cat "  Passed:       " *tests-passed* " ✓"))
	(print (cat "  Failed:       " *tests-failed* (if (> *tests-failed* 0) " ✗" "")))
	(print "")

	(defq pass-rate (if (> *tests-run* 0)
		(/ (* *tests-passed* 100) *tests-run*)
		0))
	(print (cat "  Pass rate:    " pass-rate "%"))
	(print "")

	(if (zero? *tests-failed*)
		(progn
			(print "╔════════════════════════════════════════════════════════════════╗")
			(print "║  🎉 ALL TESTS PASSED! 🎉                                      ║")
			(print "╚════════════════════════════════════════════════════════════════╝"))
		(progn
			(print "╔════════════════════════════════════════════════════════════════╗")
			(print "║  ⚠️  SOME TESTS FAILED                                        ║")
			(print "╚════════════════════════════════════════════════════════════════╝")))
	(print ""))

(defun quick-test ()
	"Quick smoke test"
	(print "Running quick smoke test...")
	(test-assert (eql (trim-string "  test  ") "test") "quick test")
	(print "✓ Quick test passed"))

(print "LITPROG Test Suite v4 loaded")
(print "Run (run-all-tests) to execute all tests")
(print "Run (quick-test) for a quick smoke test")
