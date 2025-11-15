;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; ChrysaLisp Test Runner
; Run test files with filtering and reporting
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "lib/options/options.inc")
(import "lib/test/test.inc")

(defq usage `(
(("-h" "--help")
"Usage: test-runner [options] [test_files...]

	options:
		-h --help              : this help info
		-f --filter PATTERN    : only run tests matching PATTERN
		-v --verbose           : show all test results (not just failures)
		-n --no-color          : disable colored output
		-p --pattern GLOB      : glob pattern for test files (default: examples/test/**/*_test.lisp)
		-l --list              : list available test files without running them

	examples:
		# Run all tests
		./run_tui.sh -n 1 cmd/test-runner.lisp

		# Run specific test file
		./run_tui.sh -n 1 cmd/test-runner.lisp examples/test/basic_test.lisp

		# Run tests matching a pattern
		./run_tui.sh -n 1 cmd/test-runner.lisp -f \"math\"

		# Run with verbose output
		./run_tui.sh -n 1 cmd/test-runner.lisp -v

		# Run all tests in a directory
		./run_tui.sh -n 1 cmd/test-runner.lisp -p \"examples/test/**/*_test.lisp\"
")
))

;;;;;;;;;;;;;;;;;;;;;;;;
; Options
;;;;;;;;;;;;;;;;;;;;;;;;

(defq opt_filter :nil)
(defq opt_verbose :nil)
(defq opt_no_color :nil)
(defq opt_pattern "examples/test/**/*_test.lisp")
(defq opt_list :nil)

(defq optlist `(
	(("-h" "--help") ,(first (first usage)))
	(("-f" "--filter") ,(opt-str 'opt_filter))
	(("-v" "--verbose") ,(opt-flag 'opt_verbose))
	(("-n" "--no-color") ,(opt-flag 'opt_no_color))
	(("-p" "--pattern") ,(opt-str 'opt_pattern))
	(("-l" "--list") ,(opt-flag 'opt_list))
))

;;;;;;;;;;;;;;;;;;;;;;;;
; File Discovery
;;;;;;;;;;;;;;;;;;;;;;;;

(defun find-test_files (pattern)
	; (find-test_files pattern) -> (file ...)
	; Find test files matching the pattern
	(defq files (list))
	(when (file-exists? "examples/test")
		(each! 0 -1 (lambda (entry)
			(defq path (second entry))
			(when (and
					(ends-with "_test.lisp" path)
					(not (starts-with "." (file-name path))))
				(push files path)))
			(file-tree "examples/test" (list))))
	files)

(defun list-test_files (files)
	; (list-test_files files) -> :nil
	; List available test files
	(print)
	(print "Available test files:")
	(print)
	(each! 0 -1 (lambda (file)
		(print "  " file)) files)
	(print)
	(print "Total: " (length files) " test file(s)")
	(print))

;;;;;;;;;;;;;;;;;;;;;;;;
; Test Execution
;;;;;;;;;;;;;;;;;;;;;;;;

(defun run-test-file (file)
	; (run-test-file file) -> :nil
	; Run a single test file
	(catch
		(progn
			(load file)
			:t)
		(progn
			(print)
			(print "ERROR: Failed to load test file: " file)
			(print)
			:nil)))

(defun run-test_files (files)
	; (run-test_files files) -> exit_code
	; Run all test files
	(test-reset)
	(when opt_filter
		(test-set-filter opt_filter))
	(when opt_verbose
		(test-set-verbose :t))
	(when opt_no_color
		(test-set-color :nil))

	(print)
	(print "Running " (length files) " test file(s)")
	(when opt_filter
		(print "Filter: " opt_filter))

	(each! 0 -1 run-test-file files)

	(test-run-summary)
	(test-exit-code))

;;;;;;;;;;;;;;;;;;;;;;;;
; Main Entry Point
;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	; Main entry point
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio optlist)))

		; Determine which files to test
		(defq test_files
			(if (nempty? args)
				; Use files from command line
				args
				; Use pattern to find files
				(find-test_files opt_pattern)))

		; List mode or run mode
		(if opt_list
			(progn
				(list-test_files test_files)
				0)
			(if (empty? test_files)
				(progn
					(print)
					(print "No test files found!")
					(print)
					(print "Try:")
					(print "  - Create test files in examples/test/ ending with _test.lisp")
					(print "  - Specify test files on command line")
					(print "  - Use -p to specify a different pattern")
					(print)
					1)
				(run-test_files test_files)))))
