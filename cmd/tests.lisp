(import "lib/options/options.inc")
(import "tests/suite.inc")

(defq usage `(
(("-h" "--help")
"Usage: tests [options] [path] ...

    options:
        -h --help: this help info.
        -m --match str: only the modules with str in their path.
        -l --list: list the modules, do not run them.
        -v --verbose: show every test, not just the failures.
        -f --frames: record stack frames, so an error says what
            was running. Slower.
        -j --jobs num: max modules per batch, default 1.
        -c --counts: end with a line of counts, not the summary.
            The task of a batch is run with this.

    Run the unit tests, tests/<category>/test_<name>.lisp, or
    just the module paths given.

    The modules run in parallel, a batch to a task, over the
    nodes. If they all fit in one batch they run in this task,
    one after another, so a large -j is a serial run.

    Prints the failures and a summary.")
(("-m" "--match") ,(opt-str 'opt_m))
(("-l" "--list") ,(opt-flag 'opt_l))
(("-v" "--verbose") ,(opt-flag 'opt_v))
(("-f" "--frames") ,(opt-flag 'opt_f))
(("-j" "--jobs") ,(opt-num 'opt_j))
(("-c" "--counts") ,(opt-flag 'opt_c))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_m :nil opt_l :nil opt_v :nil opt_f :nil opt_j 1 opt_c :nil
				args (options stdio usage)))
		(defq modules (if (empty? (defq modules (rest args))) (test-modules opt_m) modules))
		(cond
			(opt_l (each (const print) modules))
			((catch
				(run-suite modules opt_v opt_f opt_j opt_c)
				(progn
					(print "CRITICAL ERROR: Test suite crashed or threw exception.")
					(print "Error object: " _)
					:t))))))
