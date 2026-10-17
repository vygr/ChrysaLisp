(import "lib/options/options.inc")
(import "lib/sync/sync.inc")

(defq usage `(
(("-h" "--help")
"Usage: waypoint [options]

    options:
        -h --help: this help info.
        -c --check: say where the tree is, and change nothing.

    Note the waypoint this machine's tree is at, the file waypoint at
    its root: the commit git has it at, when, and the top of its tree
    of hashes. Run it when the tree is exactly a commit of master or of
    a release, after a git pull.

    Only a tree that is exactly at its waypoint takes part in the herd,
    a sync to or from a machine that is not one of its own: a tree with
    a change of its own since is not, and nothing is pushed to it or
    taken from it. Pull and note the waypoint again and it is back.
    lib/sync/sync.inc, Who takes a sync.

    With -c, the waypoint it was last at and if it is still exactly that.")
(("-c" "--check") ,(opt-flag 'opt_c))
))

(defun git-commit ()
	; (git-commit) -> :nil | commit
	;the commit git has the tree at: the one its HEAD names, by its ref, as
	;a file of its own or in the packed refs, or HEAD itself
	(when (defq head (load ".git/HEAD"))
		(setq head (trim head))
		(cond
			((starts-with "ref: " head)
				(defq ref (slice head 5 -1))
				(cond
					((defq one (load (cat ".git/" ref))) (trim one))
					((some (lambda (line)
							(defq parts (split line " "))
							(and (= (length parts) 2) (eql (second parts) ref) (first parts)))
						(split (ifn (load ".git/packed-refs") "") (ascii-char 10))))))
			((= (length head) 40) head))))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_c :nil args (options stdio usage)))
		(defq kept (cat "obj/" (cpu) "/" (abi) "/sync_hashes") top (sync-top "." kept)
			at (sync-waypoint "."))
		(cond
			(opt_c
				(cond
					((not at) (print "No waypoint noted, this tree takes no part in the herd."))
					((eql (third at) top)
						(print "At waypoint " (first at) ", exactly, it takes part in the herd."))
					(:t (print "Was at waypoint " (first at) ", changed since, it takes no part in the herd."))))
			((not (defq commit (git-commit)))
				(print "No commit to note, git has none here."))
			(:t (save (sync-waypoint-text commit (pii-time) top) "waypoint")
				(print "Waypoint " commit " noted, this tree takes part in the herd.")))))
