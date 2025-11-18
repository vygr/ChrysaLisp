(import "lib/options/options.inc")
(import "lib/expr/parser.inc")
(import "lib/expr/serializer.inc")
(import "lib/expr/eval.inc")
(import "lib/expr/stats.inc")

(defq usage `(
(("-h" "--help")
"Usage: expr_test [options]

	options:
		-h --help: this help info.
		-v --verbose: verbose output showing all tests.
		-q --quiet: only show failures.
		-t --timing: show timing information.

	Comprehensive test suite for expression parser/serializer.
	Tests parsing, serialization, evaluation, and statistics.")
(("-v" "--verbose") ,(opt-flag 'opt_verbose))
(("-q" "--quiet") ,(opt-flag 'opt_quiet))
(("-t" "--timing") ,(opt-flag 'opt_timing))
))

(defq *tests-passed* 0)
(defq *tests-failed* 0)
(defq *test-start-time* 0)

(defun test-start ()
	(when opt_timing
		(setq *test-start-time* (pii-time))))

(defun test-end ()
	(when opt_timing
		(defq elapsed (- (pii-time) *test-start-time*))
		(print "")
		(print "Total time: " (time-in-seconds elapsed))))

(defun test-assert (name condition)
	; (test-assert name condition)
	;assert a test condition
	(if condition
		(progn
			(when opt_verbose
				(print "[PASS] " name))
			(setq *tests-passed* (inc *tests-passed*)))
		(progn
			(unless opt_quiet
				(print "[FAIL] " name))
			(setq *tests-failed* (inc *tests-failed*)))))

(defun test-equal (name expected actual)
	; (test-equal name expected actual)
	;test equality
	(if (eql expected actual)
		(progn
			(when opt_verbose
				(print "[PASS] " name))
			(setq *tests-passed* (inc *tests-passed*)))
		(progn
			(unless opt_quiet
				(print "[FAIL] " name)
				(print "  Expected: " expected)
				(print "  Actual: " actual))
			(setq *tests-failed* (inc *tests-failed*)))))

(defun test-catch (name thunk)
	; (test-catch name thunk)
	;test that no exception is thrown
	(catch (progn
		(thunk)
		(when opt_verbose
			(print "[PASS] " name))
		(setq *tests-passed* (inc *tests-passed*))
		:t)
		(progn
			(unless opt_quiet
				(print "[FAIL] " name " - Exception: " _))
			(setq *tests-failed* (inc *tests-failed*))
			:nil)))

(defun run-parser-tests ()
	(print "")
	(print "=== Parser Tests ===")
	;test S-expression parsing
	(test-catch "Parse simple number" (lambda ()
		(expr-parse "42" 'sexp)))
	(test-catch "Parse simple symbol" (lambda ()
		(expr-parse "x" 'sexp)))
	(test-catch "Parse simple list" (lambda ()
		(expr-parse "(+ 1 2)" 'sexp)))
	(test-catch "Parse nested list" (lambda ()
		(expr-parse "(+ 1 (* 2 3))" 'sexp)))
	;test infix parsing
	(test-catch "Parse infix addition" (lambda ()
		(expr-parse "1 + 2" 'infix)))
	(test-catch "Parse infix multiplication" (lambda ()
		(expr-parse "2 * 3" 'infix)))
	(test-catch "Parse infix complex" (lambda ()
		(expr-parse "1 + 2 * 3" 'infix)))
	(test-catch "Parse infix with parens" (lambda ()
		(expr-parse "(1 + 2) * 3" 'infix)))
	;test prefix parsing
	(test-catch "Parse prefix addition" (lambda ()
		(expr-parse "+ 1 2" 'prefix)))
	(test-catch "Parse prefix multiplication" (lambda ()
		(expr-parse "* 2 3" 'prefix)))
	;test auto-detection
	(test-catch "Auto-detect sexp" (lambda ()
		(expr-parse "(+ 1 2)" 'auto)))
	(test-catch "Auto-detect infix" (lambda ()
		(expr-parse "1 + 2" 'auto))))

(defun run-serializer-tests ()
	(print "")
	(print "=== Serializer Tests ===")
	(defq expr '(+ 1 (* 2 3)))
	;test various output formats
	(test-catch "Serialize to sexp" (lambda ()
		(expr-serialize expr 'sexp :nil)))
	(test-catch "Serialize to pretty" (lambda ()
		(expr-serialize expr 'pretty :nil)))
	(test-catch "Serialize to json" (lambda ()
		(expr-serialize expr 'json :nil)))
	(test-catch "Serialize to xml" (lambda ()
		(expr-serialize expr 'xml :nil)))
	(test-catch "Serialize to dot" (lambda ()
		(expr-serialize expr 'dot :nil)))
	(test-catch "Serialize to infix" (lambda ()
		(expr-serialize expr 'infix :nil)))
	(test-catch "Serialize to prefix" (lambda ()
		(expr-serialize expr 'prefix :nil)))
	(test-catch "Serialize to ast" (lambda ()
		(expr-serialize expr 'ast :nil)))
	(test-catch "Serialize to tree" (lambda ()
		(expr-serialize expr 'tree :nil)))
	(test-catch "Serialize to rainbow" (lambda ()
		(expr-serialize expr 'rainbow :nil))))

(defun run-eval-tests ()
	(print "")
	(print "=== Evaluator Tests ===")
	;test arithmetic
	(test-equal "Eval addition" 3 (expr-eval '(+ 1 2)))
	(test-equal "Eval subtraction" 1 (expr-eval '(- 3 2)))
	(test-equal "Eval multiplication" 6 (expr-eval '(* 2 3)))
	(test-equal "Eval division" 2 (expr-eval '(/ 6 3)))
	(test-equal "Eval nested" 7 (expr-eval '(+ 1 (* 2 3))))
	;test special operations
	(test-equal "Eval factorial 5" 120 (expr-eval '(factorial 5)))
	(test-equal "Eval min" 1 (expr-eval '(min 1 2 3)))
	(test-equal "Eval max" 3 (expr-eval '(max 1 2 3)))
	(test-equal "Eval abs positive" 5 (expr-eval '(abs 5)))
	(test-equal "Eval abs negative" 5 (expr-eval '(abs -5)))
	;test comparison
	(test-equal "Eval equals true" 1 (expr-eval '(= 2 2)))
	(test-equal "Eval equals false" 0 (expr-eval '(= 2 3)))
	(test-equal "Eval less than" 1 (expr-eval '(< 2 3)))
	(test-equal "Eval greater than" 1 (expr-eval '(> 3 2))))

(defun run-stats-tests ()
	(print "")
	(print "=== Statistics Tests ===")
	(defq expr '(+ 1 (* 2 3)))
	;test statistics collection
	(test-catch "Collect stats" (lambda ()
		(expr-stats expr)))
	(test-equal "Count nodes" 5 (expr-node-count expr))
	(test-equal "Count depth" 3 (expr-depth expr))
	(test-equal "Count operators" 2 (expr-operator-count expr))
	(test-equal "Count operands" 3 (expr-operand-count expr))
	;test complexity
	(test-catch "Calculate complexity" (lambda ()
		(expr-complexity expr))))

(defun run-roundtrip-tests ()
	(print "")
	(print "=== Roundtrip Tests ===")
	;test that parsing and serializing preserves structure
	(defun test-roundtrip (name expr)
		(test-catch name (lambda ()
			(defq parsed (expr-parse expr 'sexp))
			(defq serialized (expr-serialize parsed 'sexp :nil))
			(defq reparsed (expr-parse serialized 'sexp))
			(if (eql parsed reparsed)
				:t
				(throw "Roundtrip failed" (list parsed reparsed))))))
	(test-roundtrip "Roundtrip simple" "(+ 1 2)")
	(test-roundtrip "Roundtrip nested" "(+ 1 (* 2 3))")
	(test-roundtrip "Roundtrip complex" "(+ (* 2 3) (/ 8 4))")
	;test infix roundtrip
	(defun test-infix-roundtrip (name expr)
		(test-catch name (lambda ()
			(defq parsed (expr-parse expr 'infix))
			(defq serialized (expr-serialize parsed 'infix :nil))
			(defq reparsed (expr-parse serialized 'infix))
			;structure should be equivalent
			:t)))
	(test-infix-roundtrip "Infix roundtrip simple" "1 + 2")
	(test-infix-roundtrip "Infix roundtrip precedence" "1 + 2 * 3"))

(defun run-edge-case-tests ()
	(print "")
	(print "=== Edge Case Tests ===")
	;test edge cases
	(test-catch "Empty expression handling" (lambda ()
		(expr-parse "()" 'sexp)))
	(test-catch "Single number" (lambda ()
		(expr-eval 42)))
	(test-catch "Single symbol" (lambda ()
		(expr-eval 'pi)))
	(test-equal "Zero addition" 0 (expr-eval '(+)))
	(test-equal "Zero multiplication" 1 (expr-eval '(*)))
	(test-equal "Unary minus" -5 (expr-eval '(- 5)))
	(test-catch "Large expression" (lambda ()
		(expr-eval '(+ 1 2 3 4 5 6 7 8 9 10))))
	(test-catch "Deep nesting" (lambda ()
		(expr-eval '(+ (+ (+ (+ (+ 1 2) 3) 4) 5) 6)))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_verbose :nil opt_quiet :nil opt_timing :nil
				args (options stdio usage)))
		(test-start)
		(print "")
		(print "====================================")
		(print "Expression Parser Test Suite")
		(print "====================================")
		;run all test suites
		(run-parser-tests)
		(run-serializer-tests)
		(run-eval-tests)
		(run-stats-tests)
		(run-roundtrip-tests)
		(run-edge-case-tests)
		;print summary
		(print "")
		(print "====================================")
		(print "Test Summary")
		(print "====================================")
		(print "Passed: " *tests-passed*)
		(print "Failed: " *tests-failed*)
		(print "Total: " (+ *tests-passed* *tests-failed*))
		(if (= *tests-failed* 0)
			(print "Status: ALL TESTS PASSED")
			(print "Status: SOME TESTS FAILED"))
		(test-end)
		(print "====================================")
		))
