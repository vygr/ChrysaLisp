(import "lib/options/options.inc")
(import "lib/task/pipe.inc")

(defq usage `(
(("-h" "--help")
"Usage: lint [options]

    options:
        -h --help: this help info.
        -v --verbose: say what each step took.

    The lint of the VP source, all of it in the one go.

    The trace lint works out what each function really
    trashes, and says where that is not what its header has
    written down. A normal build keeps its VP in obj/vp/,
    so this makes what has changed, 'make'
    then 'make apps', and runs
    'files obj/vp/ | trace -i -l'. And the same of the VP of
    a validate build, 'make trace validate' then
    'files obj/vp_validate/ | trace -i -l', no platform is
    built in it: a (validatecase) must use no more registers.

    Then 'relies -q', on the same build: where a
    function reads a register after a call that is declared
    to trash it, which the trace does not look for.

    Prints what the lint and the builds have to say, which
    is nothing when all is well, then a line to say so.")
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun run-step (cmd)
	; (run-step cmd) -> lines
	;what a command has to say, a line at a time
	(defq out (list) t0 (pii-time))
	(pipe-run cmd (# (push out %0)))
	(if opt_v (print cmd ": " (/ (- (pii-time) t0) 1000) "ms"))
	(filter (# (nempty? (trim %0))) (split (apply (const cat) (cat (list "") out)) "\n")))

(defun build-errors (cmd said)
	; (build-errors cmd said) -> said
	;a build says a lot, only an error is wanted, added to what has been
	;said so far
	(filter! (# (found? %0 "rror")) (run-step cmd) 0 -1 said))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_v :nil args (options stdio usage)))
		;all that is said goes into the one list, as it is said
		(defq t0 (pii-time) said (list))
		(build-errors "make" said)
		(build-errors "make apps" said)
		(each (# (push said %0)) (run-step "files obj/vp/ | trace -i -l"))
		(build-errors "make trace validate" said)
		(each (# (push said %0)) (run-step "files obj/vp_validate/ | trace -i -l"))
		;and where a register is read after a call that is declared to
		;trash it: what trace does not see, it is of what calls
		(each (# (push said %0)) (run-step "relies -q"))
		(each (const print) said)
		(print (if (empty? said) "lint: clean" "lint: see above")
			", " (/ (- (pii-time) t0) 1000) "ms")))
