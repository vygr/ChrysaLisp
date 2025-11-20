;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; FiveAM Framework - Comprehensive Self-Tests
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Comprehensive test suite for the FiveAM testing framework.
;; Uses FiveAM to test itself (meta-testing).
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/test/test.inc")

(print "=== FiveAM Comprehensive Test Suite ===" (ascii-char 10))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 1: Basic Assertions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :assertion-tests :description "Test all assertion macros")
(in-suite :assertion-tests)

(def-test is-eq-test
	:description "Test is-eq macro for equality"
	(is-eq 5 5 "Equal numbers")
	(is-eq :hello :hello "Equal symbols")
	(is-eq 0 (- 5 5) "Equal expressions"))

(def-test is-true-test
	:description "Test is-true macro"
	(is-true :t "True is true")
	(is-true (< 1 2) "1 less than 2")
	(is-true (not :nil) "Not nil is true"))

(def-test is-false-test
	:description "Test is-false macro"
	(is-false :nil "Nil is false")
	(is-false (> 1 2) "1 not greater than 2")
	(is-false (eql 5 6) "5 not equal 6"))

(def-test is-equal-test
	:description "Test is-equal macro for deep equality"
	(is-equal (list 1 2 3) (list 1 2 3) "Lists are equal")
	(is-equal "hello" "hello" "Strings are equal"))

(def-test explicit-pass-fail-skip-test
	:description "Test explicit result macros"
	(pass "Explicit pass works")
	(skip "Skipping this check"))

(def-test finishes-test
	:description "Test finishes macro"
	(finishes
		(+ 1 1)
		(* 2 3)
		(cat "hello" " " "world")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 2: Test Definition and Registry
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :registry-tests :description "Test registration and retrieval")
(in-suite :registry-tests)

(def-test test-registration
	:description "Verify tests are registered correctly"
	(def-test temp-test-for-registry
		(pass "Temporary test"))

	(defq test-obj (get-test 'temp-test-for-registry))
	(is-true (not (eql test-obj :nil)) "Test was registered")

	(when test-obj
		(is-eq :test-case (. test-obj :find :type) "Test has correct type")
		(is-eq 'temp-test-for-registry (. test-obj :find :name) "Test has correct name")))

(def-test suite-registration
	:description "Verify suites are registered correctly"
	(def-suite :temp-suite-for-registry :description "Temporary suite")

	(defq suite-obj (get-test :temp-suite-for-registry))
	(is-true (not (eql suite-obj :nil)) "Suite was registered")

	(when suite-obj
		(is-eq :test-suite (. suite-obj :find :type) "Suite has correct type")
		(is-eq :temp-suite-for-registry (. suite-obj :find :name) "Suite has correct name")))

(def-test test-removal
	:description "Verify tests can be removed"
	(def-test temp-test-to-remove
		(pass "Will be removed"))

	(is-true (not (eql (get-test 'temp-test-to-remove) :nil)) "Test exists before removal")

	(rem-test 'temp-test-to-remove)
	(is-eq :nil (get-test 'temp-test-to-remove) "Test removed successfully"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 3: Test Execution
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :execution-tests :description "Test execution and result collection")
(in-suite :execution-tests)

(def-test result-collection
	:description "Verify results are collected during test execution"
	(clear-results)

	(def-test temp-test-with-results
		(is-eq 1 1)
		(is-true :t)
		(pass))

	(defq results (run 'temp-test-with-results))
	(is-eq 3 (length results) "Three results collected"))

(def-test result-types
	:description "Verify different result types are created"
	(clear-results)

	(def-test temp-test-mixed-results
		(is-eq 1 1)        ; pass
		(skip "Skipping")  ; skip
		(is-true :t))      ; pass

	(defq results (run 'temp-test-mixed-results))

	(defq pass-count 0 skip-count 0)
	(each (lambda (result)
		(defq type (. result :find :type))
		(cond
			((eql type :passed) (setq pass-count (inc pass-count)))
			((eql type :skipped) (setq skip-count (inc skip-count)))))
		results)

	(is-eq 2 pass-count "Two passes")
	(is-eq 1 skip-count "One skip"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 4: Test Dependencies
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :dependency-tests :description "Test dependency resolution")
(in-suite :dependency-tests)

(def-test prereq-test-always-passes
	:description "Prerequisite that always passes"
	(is-true :t "Always passes"))

(def-test prereq-test-always-fails
	:description "Prerequisite that always fails"
	(is-eq 1 2 "Always fails"))

(def-test dependent-on-passing-test
	:description "Test that depends on passing prerequisite"
	:depends-on 'prereq-test-always-passes
	(is-true :t "Should run because prerequisite passed"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 5: Suite Organization
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :suite-organization-tests :description "Test suite organization features")
(in-suite :suite-organization-tests)

(def-test suite-creation
	:description "Verify suites can be created and configured"
	(def-suite :nested-test-suite
		:description "A nested suite for testing"
		:in :suite-organization-tests)

	(defq suite (get-test :nested-test-suite))
	(is-true (not (eql suite :nil)) "Suite was created")

	(when suite
		(is-equal "A nested suite for testing"
			(. suite :find :description)
			"Suite has correct description")))

(def-test in-suite-changes-context
	:description "Verify in-suite changes current suite"
	(defq original-suite *current-suite*)

	(def-suite :temp-context-suite)
	(in-suite :temp-context-suite)

	(is-eq :temp-context-suite *current-suite* "Current suite changed")

	;; Restore original context
	(setq *current-suite* original-suite))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Test Suite 6: Result Analysis
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(def-suite :result-analysis-tests :description "Test result analysis functions")
(in-suite :result-analysis-tests)

(def-test result-status-analysis
	:description "Verify results-status function works"
	(clear-results)

	(def-test temp-analysis-test
		(is-eq 1 1)
		(is-eq 2 2)
		(skip "Skip this"))

	(defq results (run 'temp-analysis-test))
	(defq status-r1 (results-status results))
	(defq all-passed (elem 0 status-r1))
	(defq failed (elem 1 status-r1))
	(defq skipped (elem 2 status-r1))

	(is-eq 0 (length failed) "No failures")
	(is-eq 1 (length skipped) "One skip"))

(def-test partition-results-test
	:description "Verify partition-results function works"
	(clear-results)

	(def-test temp-partition-test
		(pass)
		(pass)
		(skip))

	(defq results (run 'temp-partition-test))
	(defq part-r1 (partition-results results))
	(defq num-total (elem 0 part-r1))
	(defq passed (elem 1 part-r1))
	(defq failed (elem 2 part-r1))
	(defq skipped (elem 3 part-r1))
	(defq errors (elem 4 part-r1))

	(is-eq 3 num-total "Three total results")
	(is-eq 2 (length passed) "Two passes")
	(is-eq 0 (length failed) "Zero failures")
	(is-eq 1 (length skipped) "One skip"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Run All Test Suites
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(print (ascii-char 10) "Running all test suites..." (ascii-char 10) (ascii-char 10))

;; Run each suite
(defq all-suites (list
	:assertion-tests
	:registry-tests
	:execution-tests
	:dependency-tests
	:suite-organization-tests
	:result-analysis-tests))

(defq total-passed 0 total-failed 0 total-skipped 0)

(each (lambda (suite-name)
	(print "=== Running suite: " suite-name " ===" (ascii-char 10))
	(defq results (run suite-name))
	(defq status-r2 (results-status results))
	(defq all-ok (elem 0 status-r2))
	(defq failed (elem 1 status-r2))
	(defq skipped (elem 2 status-r2))

	(defq part-r2 (partition-results results))
	(defq num-total (elem 0 part-r2))
	(defq passed (elem 1 part-r2))
	(defq failed-list (elem 2 part-r2))
	(defq skipped-list (elem 3 part-r2))
	(defq errors (elem 4 part-r2))
	(setq total-passed (+ total-passed (length passed)))
	(setq total-failed (+ total-failed (length failed-list) (length errors)))
	(setq total-skipped (+ total-skipped (length skipped-list)))

	(explain-simple results)
	(print (ascii-char 10)))
	all-suites)

;; Final summary
(print "========================================" (ascii-char 10))
(print "FINAL RESULTS" (ascii-char 10))
(print "========================================" (ascii-char 10))
(print "Total Passed:  " total-passed (ascii-char 10))
(print "Total Failed:  " total-failed (ascii-char 10))
(print "Total Skipped: " total-skipped (ascii-char 10))
(print "========================================" (ascii-char 10))

(if (= 0 total-failed)
	(print "SUCCESS: All tests passed!" (ascii-char 10))
	(print "FAILURE: Some tests failed!" (ascii-char 10)))
