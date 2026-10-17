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
        -x --skip machines: machines not to touch at all, not brought
            level and not run on, a guest's in the mesh say, the same way.
        -o --only ids: the only machines to touch, the start of the
            system id of each, mesh lists them, with a , between: one's
            own rig, with machines of others in the mesh.
        -d --delete paths: files to remove on the other machines, with
            a : between.
        -u --user name: the sessions are that user's, a folder of usr/,
            and not whoever last signed on to each machine.
        -g --gui: each session is a desktop, its GUI up before the
            command is run, on the machine's own screen.
        -v --verbose: every line each session said, not only those
            of how the tests went.

    Run a command line on every machine of the mesh. Each machine that
    takes a sync, sync -a, is first made the same as this one. Then each,
    and this one, starts a session of its own, new, sized to itself, runs
    the command, and the session ends. Command lines with a ; between
    are run one after another in the one session.

    A line comes back for each machine.

        rack \qtests -a\q     ; every test, on every machine
        rack -b tests       ; after a change to the VP code")
(("-b" "--build") ,(opt-flag 'opt_b))
(("-l" "--leave") ,(opt-str 'opt_l))
(("-x" "--skip") ,(opt-str 'opt_x))
(("-o" "--only") ,(opt-str 'opt_o))
(("-d" "--delete") ,(opt-str 'opt_d))
(("-u" "--user") ,(opt-str 'opt_u))
(("-g" "--gui") ,(opt-flag 'opt_g))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(and
			(defq stdio (create-stdio))
			(defq opt_b :nil opt_l "" opt_x "" opt_o :nil opt_d "" opt_u :nil opt_g :nil opt_v :nil
				args (options stdio usage))
		(if (<= (length args) 1)
			(print "A command line to run is needed, rack -h.")
			(each (const print) (rack-run (join (rest args) " ")
				opt_b (split opt_l ",") (split opt_d ":") opt_u :nil opt_g opt_v (split opt_x ",") (if opt_o (split opt_o ",")))))))
