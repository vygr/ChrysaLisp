(import "lib/options/options.inc")
(import "lib/rack/rack.inc")

(defq usage `(
(("-h" "--help")
"Usage: rack [options] \qcommand line\q

    options:
        -h --help: this help info.
        -b --build: make, and make all boot, on each machine first, in a
            session before the one that runs the command.
        -l --leave machines: machines to bring level and not run on, as
            sync lists them, arm64/Linux, with a , between.
        -d --delete paths: files to remove on the other machines, with
            a : between.

    Run a command line on every machine of the mesh. Each machine that
    takes a sync, sync -a, is first made the same as this one. Then each,
    and this one, starts a session of its own, new, sized to itself, runs
    the command, and the session ends.

    A line comes back for each machine.

        rack \qtests -a\q     ; every test, on every machine
        rack -b tests       ; after a change to the VP code")
(("-b" "--build") ,(opt-flag 'opt_b))
(("-l" "--leave") ,(opt-str 'opt_l))
(("-d" "--delete") ,(opt-str 'opt_d))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_b :nil opt_l "" opt_d ""
				args (options stdio usage)))
		(if (<= (length args) 1)
			(print "A command line to run is needed, rack -h.")
			(each (const print) (rack-run (join (rest args) " ")
				opt_b (split opt_l ",") (split opt_d ":"))))))
