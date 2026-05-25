(import "lib/options/options.inc")
(import "tests/run_all.lisp")

(defq usage `(
(("-h" "--help")
"Usage: tests [options]

    options:
        -h --help: this help info.

    Run the unit tests.")
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq args (options stdio usage)))
		(catch
			(run-suite)
			(progn
				(print "CRITICAL ERROR: Test suite crashed or threw exception.")
				(print "Error object: " _)
				:t))))
