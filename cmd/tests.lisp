(import "lib/options/options.inc")
(import "tests/suite.inc")

(defq usage `(
(("-h" "--help")
"Usage: tests [options]

    options:
        -h --help: this help info.
        -m --match str: only the modules with str in their path.
        -l --list: list the modules, do not run them.
        -v --verbose: show every test, not just the failures.

    Run the unit tests, tests/<category>/test_<name>.lisp.

    Prints the failures and a summary.")
(("-m" "--match") ,(opt-str 'opt_m))
(("-l" "--list") ,(opt-flag 'opt_l))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_m :nil opt_l :nil opt_v :nil args (options stdio usage)))
		(if opt_l
			(each (const print) (test-modules opt_m))
			(catch
				(run-suite opt_m opt_v)
				(progn
					(print "CRITICAL ERROR: Test suite crashed or threw exception.")
					(print "Error object: " _)
					:t)))))
