(import "lib/options/options.inc")
(import "lib/sync/sync.inc")

(defq usage `(
(("-h" "--help")
"Usage: sync [options]

    options:
        -h --help: this help info.
        -a --accept: this machine will take a sync, till it is told
            not to or its session ends.
        -x --stop: this machine will no longer take a sync.
        -t --to id: make that machine's tree the same as this one's.
            The start of its id, as the list shows it, or all.
        -c --check: with -t, say what differs and change nothing.
        -d --delete: with -t, remove what is there and not here.
        -r --root path: the root of the tree, default the system's own.
        -v --verbose: name each file.

    Make the files of another machine the same as the files of this one,
    over the links between them. Only what differs is sent. What the
    .gitignore of the tree leaves out is not sent, or removed.

    A machine only takes a sync if it was told to, sync -a.

    With no options, list the machines that will take one.

        sync -a           ; on the machine to be updated
        sync              ; on this one, who will take a sync ?
        sync -t all -c    ; what would change on them
        sync -t D649      ; send it to the one")
(("-a" "--accept") ,(opt-flag 'opt_a))
(("-x" "--stop") ,(opt-flag 'opt_x))
(("-t" "--to") ,(opt-str 'opt_t))
(("-c" "--check") ,(opt-flag 'opt_c))
(("-d" "--delete") ,(opt-flag 'opt_d))
(("-r" "--root") ,(opt-str 'opt_r))
(("-v" "--verbose") ,(opt-flag 'opt_v))
))

(defun main ()
	;initialize pipe details and command args, abort on error
	(when (and
			(defq stdio (create-stdio))
			(defq opt_a :nil opt_x :nil opt_t :nil opt_c :nil opt_d :nil opt_r "." opt_v :nil
				args (options stdio usage)))
		(defq me (hex-encode (system-id)) services (sync-services)
			here (filter (# (eql (second %0) me)) services)
			there (filter (# (nql (second %0) me)) services))
		(cond
			(opt_x
				(each (# (sync-tell (first %0) (defq mbox (mail-mbox)) +sync_type_quit "")
					(sync-hear mbox 2000000)) here)
				(print (if (empty? here) "This machine was not taking a sync." "This machine no longer takes a sync.")))
			(opt_a
				(cond
					((nempty? here) (print "This machine takes a sync already, under " (last (first here))))
					((not (or (eql opt_r ".") (and (sync-safe? opt_r) (sync-inside? "." (cat opt_r "/x")))))
						(print "A sync is only taken under the system's own tree, not " opt_r))
					(:t (mail-send (open-child "service/sync/app.lisp" +kn_call_pin)
							(cat "*Sync" (ascii-char 10) opt_r))
						(print "This machine will take a sync, under " opt_r))))
			(opt_t
				(defq targets (if (eql opt_t "all") there
					(filter (# (starts-with (to-upper opt_t) (second %0))) services)))
				(if (empty? targets) (print "No machine that takes a sync is " opt_t))
				(defq rules_text (ifn (load (cat opt_r "/.gitignore")) "")
					kept (if (eql opt_r ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes")))
				(each (lambda ((svc id machine root))
					;a host with no modes for its files is not sent them, or asked for them
					(defq t0 (pii-time) result (sync-push svc opt_r rules_text opt_c opt_d kept
						(or (eql (os) 'Windows) (ends-with "/Windows" machine))))
					(cond
						((not result) (print (slice id 0 8) " " machine " did not answer"))
						((eql result :old) (print (slice id 0 8) " " machine
							" has an older sync, it must be updated another way, and its sync started again"))
						(:t (bind '(sent bytes removed failed send gone remoded) result)
							(when (or opt_v opt_c)
								(each (# (print "  " (if opt_c "differs " "sent ") %0)) send)
								(each (# (print "  " (if opt_d (if opt_c "would remove " "removed ") "only there ") %0)) gone))
							(print (slice id 0 8) " " machine " "
								(if opt_c
									(cat (str (length send)) " differ, " (str (length gone)) " only there"
										(if (> remoded 0) (cat ", " (str remoded) " with another mode") ""))
									(cat (str sent) " sent, " (str bytes) " bytes, " (str removed) " removed"
										(if (> remoded 0) (cat ", " (str remoded) " given their mode") "")
										(if (> failed 0) (cat ", " (str failed) " FAILED") "")
										(if (and (not opt_d) (nempty? gone)) (cat ", " (str (length gone)) " only there") "")))
								", " (str (/ (- (pii-time) t0) 1000)) "ms"))))
					targets))
			(:t (if (empty? services) (print "No machine takes a sync. Run sync -a on one that will."))
				;is each one's tree the same as this one's ? One number from
				;each says, the top of its tree of hashes
				(defq rules_text (ifn (load (cat opt_r "/.gitignore")) "")
					kept (if (eql opt_r ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes"))
					mine (if (nempty? there) (Fmap 3)))
				(each (lambda ((svc id machine root))
					(print (slice id 0 8) " " machine " " root
						(cond
							((eql id me) " (this machine)")
							(:t (defq no_modes (or (eql (os) 'Windows) (ends-with "/Windows" machine))
									theirs (sync-root svc rules_text no_modes 20000000))
								;my own top, worked out the once, with modes or without
								(unless (. mine :find no_modes)
									(. mine :insert no_modes (hash-tree-root
										(sync-tree opt_r (sync-rules rules_text) kept no_modes))))
								(cond
									((not theirs) ", no answer")
									((eql theirs :old) ", an older sync")
									((eql theirs (. mine :find no_modes)) ", the same as this one")
									(:t ", differs from this one"))))))
					services)))))
