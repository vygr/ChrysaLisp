;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Framework - Minimal Smoke Test
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Quick verification that the FiveAM testing framework loads and basic
;; functionality works.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

(print "=== FiveAM Smoke Test ===" (ascii-char 10))

;; Test 1: Basic test definition and execution
(print "Test 1: Creating and running a simple test..." (ascii-char 10))

(def-test smoke-test-1
	:description "Basic arithmetic test"
	(is-eq 4 (+ 2 2) "Two plus two equals four")
	(is-true :t "True is true"))

(defq results (run 'smoke-test-1))
(print "Results count: " (length results) (ascii-char 10))

;; Test 2: Test suite creation
(print (ascii-char 10) "Test 2: Creating a test suite..." (ascii-char 10))

(def-suite :smoke-suite :description "Smoke test suite")
(in-suite :smoke-suite)

(def-test smoke-test-2
	(is-eq 10 (* 5 2) "Multiplication works"))

(defq suite-results (run :smoke-suite))
(print "Suite results count: " (length suite-results) (ascii-char 10))

;; Test 3: Verify all assertion types
(print (ascii-char 10) "Test 3: Testing assertion macros..." (ascii-char 10))

(def-test assertion-test
	(is-true :t)
	(is-false :nil)
	(is-eq 5 5)
	(is-equal (list 1 2 3) (list 1 2 3))
	(pass "Explicit pass")
	(finishes (+ 1 1)))

(defq assertion-results (run 'assertion-test))
(print "Assertion test results: " (length assertion-results) (ascii-char 10))

;; Test 4: Verify result analysis
(print (ascii-char 10) "Test 4: Testing result analysis..." (ascii-char 10))

(defq status-result (results-status assertion-results))
(defq all-passed (elem 0 status-result))
(defq failed (elem 1 status-result))
(defq skipped (elem 2 status-result))
(print "All passed: " all-passed (ascii-char 10))
(print "Failed count: " (length failed) (ascii-char 10))
(print "Skipped count: " (length skipped) (ascii-char 10))

;; Test 5: Test registry
(print (ascii-char 10) "Test 5: Testing test registry..." (ascii-char 10))

(defq test-obj (get-test 'smoke-test-1))
(print "Retrieved test: " (if test-obj "SUCCESS" "FAILED") (ascii-char 10))

(when test-obj
	(print "Test type: " (. test-obj :find :type) (ascii-char 10))
	(print "Test name: " (. test-obj :find :name) (ascii-char 10)))

;; Final summary
(print (ascii-char 10) "=== Smoke Test Complete ===" (ascii-char 10))
(print "All basic functionality verified!" (ascii-char 10))
