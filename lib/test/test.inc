;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Test Framework (describe/it/should)
; A comprehensive testing framework with BDD-style syntax
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;module
(env-push)

(import "lib/collections/collections.inc")

;;;;;;;;;;;;;;;;;;;;;;;;
; Test State Management
;;;;;;;;;;;;;;;;;;;;;;;;

(defq *test_context* (Fmap 10))
(. *test_context* :insert 'suites (list))
(. *test_context* :insert 'current_suite :nil)
(. *test_context* :insert 'current_test :nil)
(. *test_context* :insert 'total_tests 0)
(. *test_context* :insert 'passed_tests 0)
(. *test_context* :insert 'failed_tests 0)
(. *test_context* :insert 'skipped_tests 0)
(. *test_context* :insert 'failures (list))
(. *test_context* :insert 'filter_pattern :nil)
(. *test_context* :insert 'focus_mode :nil)
(. *test_context* :insert 'verbose :nil)
(. *test_context* :insert 'color_output :t)

;;;;;;;;;;;;;;;;;;;;;;;;
; Color Output Support
;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-color-reset () (if (. *test_context* :find 'color_output) "\033[0m" ""))
(defun test-color-red () (if (. *test_context* :find 'color_output) "\033[31m" ""))
(defun test-color-green () (if (. *test_context* :find 'color_output) "\033[32m" ""))
(defun test-color-yellow () (if (. *test_context* :find 'color_output) "\033[33m" ""))
(defun test-color-blue () (if (. *test_context* :find 'color_output) "\033[34m" ""))
(defun test-color-gray () (if (. *test_context* :find 'color_output) "\033[90m" ""))
(defun test-color-bold () (if (. *test_context* :find 'color_output) "\033[1m" ""))

;;;;;;;;;;;;;;;;;;;;;;;;
; Utility Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-print (&rest args)
	; (test-print arg ...) -> :nil
	; Print to stdout with flush
	(defq stream (io-stream 'stdout))
	(write-blk stream (apply str (cat args (list (ascii-char 10)))))
	(stream-flush stream))

(defun test-increment (key)
	; (test-increment key) -> :nil
	; Increment a counter in test context
	(defq current (. *test_context* :find key))
	(. *test_context* :insert key (inc current)))

(defun test-get (key)
	; (test-get key) -> val
	; Get value from test context
	(. *test_context* :find key))

(defun test-set (key val)
	; (test-set key val) -> :nil
	; Set value in test context
	(. *test_context* :insert key val))

(defun test-add-failure (suite_name test_name message)
	; (test-add-failure suite_name test_name message) -> :nil
	; Record a test failure
	(defq failures (test-get 'failures))
	(push failures (list suite_name test_name message))
	(test-set 'failures failures))

(defun test-matches-filter? (description)
	; (test-matches-filter? description) -> :t | :nil
	; Check if test matches filter pattern
	(defq pattern (test-get 'filter_pattern))
	(if pattern
		(find pattern description)
		:t))

;;;;;;;;;;;;;;;;;;;;;;;;
; Assertion Functions
;;;;;;;;;;;;;;;;;;;;;;;;

(defun should-equal (actual expected &optional message)
	; (should-equal actual expected [message]) -> :t | :nil
	; Assert that actual equals expected
	(if (= actual expected)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected " (str expected) " but got " (str actual))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-not-equal (actual expected &optional message)
	; (should-not-equal actual expected [message]) -> :t | :nil
	; Assert that actual does not equal expected
	(if (/= actual expected)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected not to equal " (str expected))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-true (actual &optional message)
	; (should-be-true actual [message]) -> :t | :nil
	; Assert that actual is true
	(if actual
		:t
		(progn
			(defq msg (if message message "Expected true but got false"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-false (actual &optional message)
	; (should-be-false actual [message]) -> :t | :nil
	; Assert that actual is false
	(if (not actual)
		:t
		(progn
			(defq msg (if message message "Expected false but got true"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-nil (actual &optional message)
	; (should-be-nil actual [message]) -> :t | :nil
	; Assert that actual is nil
	(if (eql actual :nil)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected :nil but got " (str actual))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-not-be-nil (actual &optional message)
	; (should-not-be-nil actual [message]) -> :t | :nil
	; Assert that actual is not nil
	(if (not (eql actual :nil))
		:t
		(progn
			(defq msg (if message message "Expected non-nil value"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-less-than (actual expected &optional message)
	; (should-be-less-than actual expected [message]) -> :t | :nil
	; Assert that actual is less than expected
	(if (< actual expected)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected " (str actual) " to be less than " (str expected))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-greater-than (actual expected &optional message)
	; (should-be-greater-than actual expected [message]) -> :t | :nil
	; Assert that actual is greater than expected
	(if (> actual expected)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected " (str actual) " to be greater than " (str expected))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-contain (collection item &optional message)
	; (should-contain collection item [message]) -> :t | :nil
	; Assert that collection contains item
	(if (find item collection)
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected collection to contain " (str item))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-not-contain (collection item &optional message)
	; (should-not-contain collection item [message]) -> :t | :nil
	; Assert that collection does not contain item
	(if (not (find item collection))
		:t
		(progn
			(defq msg (if message
				message
				(cat "Expected collection not to contain " (str item))))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-be-empty (collection &optional message)
	; (should-be-empty collection [message]) -> :t | :nil
	; Assert that collection is empty
	(if (empty? collection)
		:t
		(progn
			(defq msg (if message message "Expected empty collection"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-not-be-empty (collection &optional message)
	; (should-not-be-empty collection [message]) -> :t | :nil
	; Assert that collection is not empty
	(if (not (empty? collection))
		:t
		(progn
			(defq msg (if message message "Expected non-empty collection"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-throw (func &optional message)
	; (should-throw func [message]) -> :t | :nil
	; Assert that func throws an error
	(defq threw :nil)
	(catch (progn (func) :nil) (setq threw :t))
	(if threw
		:t
		(progn
			(defq msg (if message message "Expected function to throw"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

(defun should-not-throw (func &optional message)
	; (should-not-throw func [message]) -> :t | :nil
	; Assert that func does not throw an error
	(defq threw :nil)
	(catch (progn (func) :nil) (setq threw :t))
	(if (not threw)
		:t
		(progn
			(defq msg (if message message "Expected function not to throw"))
			(test-add-failure
				(test-get 'current_suite)
				(test-get 'current_test)
				msg)
			(test-increment 'failed_tests)
			:nil)))

;;;;;;;;;;;;;;;;;;;;;;;;
; Test Macros
;;;;;;;;;;;;;;;;;;;;;;;;

(defmacro it (description &rest body)
	; (it description &rest body) -> :nil
	; Define a test case
	`(progn
		(when (test-matches-filter? ,description)
			(test-set 'current_test ,description)
			(test-increment 'total_tests)
			(defq test-passed :t)
			(defq initial-failures (test-get 'failed_tests))
			~body
			(if (= initial-failures (test-get 'failed_tests))
				(progn
					(test-increment 'passed_tests)
					(when (test-get 'verbose)
						(test-print
							(test-color-green) "  ✓ " (test-color-reset)
							,description)))
				(test-print
					(test-color-red) "  ✗ " (test-color-reset)
					,description)))))

(defmacro xit (description &rest body)
	; (xit description &rest body) -> :nil
	; Define a skipped test case
	`(progn
		(when (test-matches-filter? ,description)
			(test-increment 'total_tests)
			(test-increment 'skipped_tests)
			(test-print
				(test-color-yellow) "  ○ " (test-color-reset)
				,description (test-color-gray) " (skipped)" (test-color-reset)))))

(defmacro describe (description &rest body)
	; (describe description &rest body) -> :nil
	; Define a test suite
	`(progn
		(test-set 'current_suite ,description)
		(test-print)
		(test-print (test-color-bold) ,description (test-color-reset))
		~body))

(defmacro xdescribe (description &rest body)
	; (xdescribe description &rest body) -> :nil
	; Define a skipped test suite
	`(progn
		(test-print)
		(test-print
			(test-color-bold) ,description (test-color-reset)
			(test-color-gray) " (skipped)" (test-color-reset))))

;;;;;;;;;;;;;;;;;;;;;;;;
; Test Reporting
;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-print-summary ()
	; (test-print-summary) -> :nil
	; Print test summary
	(defq total (test-get 'total_tests))
	(defq passed (test-get 'passed_tests))
	(defq failed (test-get 'failed_tests))
	(defq skipped (test-get 'skipped_tests))
	(test-print)
	(test-print (test-color-bold) "Test Summary:" (test-color-reset))
	(test-print "  Total:   " total)
	(test-print (test-color-green) "  Passed:  " passed (test-color-reset))
	(when (> failed 0)
		(test-print (test-color-red) "  Failed:  " failed (test-color-reset)))
	(when (> skipped 0)
		(test-print (test-color-yellow) "  Skipped: " skipped (test-color-reset)))
	(test-print))

(defun test-print-failures ()
	; (test-print-failures) -> :nil
	; Print detailed failure information
	(defq failures (test-get 'failures))
	(when (not (empty? failures))
		(test-print (test-color-bold) (test-color-red) "Failures:" (test-color-reset))
		(test-print)
		(each! 0 -1 (lambda ((suite test message))
			(test-print (test-color-red) "  ✗ " (test-color-reset)
				(test-color-bold) suite (test-color-reset)
				" > " test)
			(test-print "    " (test-color-red) message (test-color-reset))
			(test-print)) failures)))

(defun test-reset ()
	; (test-reset) -> :nil
	; Reset test state for a new test run
	(test-set 'current_suite :nil)
	(test-set 'current_test :nil)
	(test-set 'total_tests 0)
	(test-set 'passed_tests 0)
	(test-set 'failed_tests 0)
	(test-set 'skipped_tests 0)
	(test-set 'failures (list)))

(defun test-run-summary ()
	; (test-run-summary) -> :nil
	; Print test run summary and failures
	(test-print-failures)
	(test-print-summary))

(defun test-exit-code ()
	; (test-exit-code) -> exit_code
	; Return exit code based on test results
	(if (> (test-get 'failed_tests) 0) 1 0))

;;;;;;;;;;;;;;;;;;;;;;;;
; Configuration
;;;;;;;;;;;;;;;;;;;;;;;;

(defun test-set-filter (pattern)
	; (test-set-filter pattern) -> :nil
	; Set filter pattern for tests
	(test-set 'filter_pattern pattern))

(defun test-set-verbose (verbose)
	; (test-set-verbose verbose) -> :nil
	; Set verbose mode
	(test-set 'verbose verbose))

(defun test-set-color (color)
	; (test-set-color color) -> :nil
	; Set color output mode
	(test-set 'color_output color))

;module
(export-symbols '(
	describe xdescribe it xit
	should-equal should-not-equal
	should-be-true should-be-false
	should-be-nil should-not-be-nil
	should-be-less-than should-be-greater-than
	should-contain should-not-contain
	should-be-empty should-not-be-empty
	should-throw should-not-throw
	test-run-summary test-exit-code test-reset
	test-set-filter test-set-verbose test-set-color
))
(env-pop)
