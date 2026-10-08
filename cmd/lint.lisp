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

(defun build-errors (cmd said)
	; (build-errors cmd said) -> said
	;a build says a lot, only an error is wanted, added to what has been
	;said so far
	(filter! (# (found? %0 "rror")) (run-step cmd) 0 -1 said))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_k :nil opt_v :nil args (options stdio usage)))
		;all that is said goes into the one list, as it is said
		(defq t0 (pii-time) said (list))
		(build-errors "make vp" said)
		(build-errors "make apps debug" said)
		(each (# (push said %0)) (run-step "files obj/vp/ | trace -i -l"))
		(unless opt_k
			(build-errors "make apps" said)
			(build-errors "make all boot" said))
		(each (const print) said)
		(print (if (empty? said) "lint: clean" "lint: see above")
			", " (/ (- (pii-time) t0) 1000) "ms"
			(if opt_k ", the debug build is left" ""))))
