(import "lib/options/options.inc")
(import "lib/task/pipe.inc")

(defq usage `(
(("-h" "--help")
"Usage: lint [options]

    options:
        -h --help: this help info.
        -k --keep: leave the debug build, do not make the
            release build again after.
        -v --verbose: say what each step took.

    The lint of the VP source, all of it in the one go.

    The trace lint works out what each function really
    trashes, and says where that is not what its header has
    written down. It is only right on a debug build, so this
    makes one, 'make vp' then 'make apps debug', runs
    'files obj/vp/ | trace -i -l', and puts the release
    build back, 'make apps' then 'make all boot'.

    Prints what the lint and the builds have to say, which
    is nothing when all is well, then a line to say so.")
(("-k" "--keep") ,(opt-flag 'opt_k))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun run-step (cmd)
	; (run-step cmd) -> lines
	;what a command has to say, a line at a time
	(defq out (list) t0 (pii-time))
	(pipe-run cmd (# (push out %0)))
	(if opt_v (print cmd ": " (/ (- (pii-time) t0) 1000) "ms"))
	(filter (# (nempty? (trim %0))) (split (apply (const cat) (cat (list "") out)) (ascii-char 10))))

(defun build-errors (cmd)
	; (build-errors cmd) -> lines
	;a build says a lot, only an error is wanted
	(filter (# (found? %0 "rror")) (run-step cmd)))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_k :nil opt_v :nil args (options stdio usage)))
		(defq t0 (pii-time)
			said (cat (build-errors "make vp") (build-errors "make apps debug")
				(run-step "files obj/vp/ | trace -i -l")
				(if opt_k (list) (cat (build-errors "make apps") (build-errors "make all boot")))))
		(each (const print) said)
		(print (if (empty? said) "lint: clean" "lint: see above")
			", " (/ (- (pii-time) t0) 1000) "ms"
			(if opt_k ", the debug build is left" ""))))
