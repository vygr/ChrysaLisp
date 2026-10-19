;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; lib/rack/rack.inc - one command, on every machine of the mesh
;
; The machines that take a sync are made the same as this one, and then
; each, this one as well, runs the command in a session of its own, new,
; sized to itself, on the boot image it then has. It is how a change is
; tested on every machine there is. The session is lib/rack/session.lisp,
; and lib/rack/member.lisp is a node that stays up to be the machine's
; place in the mesh. See docs/ai_digest/rack.md.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "././task/pipe.inc")
(import "././sync/sync.inc")

;the files a session is given its commands in, and leaves what they said
;in. Each session has a pair of its own, the name of the run is put on the
;end of these, so more than one can be going on a machine: a rack run that
;is itself run by a rack run, as the test of this is, or two at once
(defq +rack_cmds "/tmp/chrysalisp_rack_cmds" +rack_out "/tmp/chrysalisp_rack_out"
	;and while it is waited for, the time, written each second by what waits
	;for it: a session whose asker has gone, stopped by hand or by a fault,
	;stops too, lib/rack/watch.lisp, and leaves no desktop up
	+rack_alive "/tmp/chrysalisp_rack_alive" +rack_stale 15000000)

(defun rack-session-clear ()
	; (rack-session-clear)
	;the nodes a session started stopped, and its note, and the note of
	;every node it names, and of those they name: a node of the session
	;that started nodes of its own left one, a test that starts a node which
	;starts six more, and nothing else clears it away. The files of the
	;links they name go with them. A list of those still to look at, this
	;does not call itself
	(defq me (task-nodeid))
	(each (lambda (node) (unless (eql node me) (open-task "(pii-exit)" node +kn_call_pin 0 (mail-mbox)))) (lisp-nodes :t))
	(defq todo (list (pii-pid)) seen (list))
	(while (defq pid (pop todo))
		(unless (find pid seen)
			(push seen pid)
			(defq file (cat "/tmp/chrysalisp_" (str pid) ".session"))
			(when (defq text (load file))
				(each (lambda (line)
					(defq parts (split line " "))
					(cond
						((< (length parts) 2))
						((eql (first parts) "link") (pii-remove (cat "/tmp/" (second parts))))
						((find (first parts) '("pid" "front")) (push todo (str-as-num (second parts))))))
					(split text (ascii-char 10)))
				(pii-remove file)))))

(defun rack-fresh (phases &optional user gui asker)
	; (rack-fresh phases [user gui asker]) -> str
	;on this machine, a new session for each phase, a list of command
	;lines, one after another, and what they said. A session is started by
	;the host, so it is on the boot image as it is on disk now, which a
	;phase before it may have made. With a user the session is theirs, and
	;not whoever last signed on to the machine, as run.sh -u has it. With
	;gui the session is on the GUI host, a desktop, its GUI up before the
	;command lines are run, as run.sh -f has it. With asker, a mailbox,
	;what a session says is mailed to it as it comes, "LINE text", and
	;if its node has gone the session is let go, and stops
	;the form has no space or tab in it, a node's args are split at those
	(defq acc (list) tab (ascii-char 11))
	(each (lambda (cmds)
		;the name of this run, the session is told it as it is told its user
		(defq run (cat "_" (str (pii-pid)) "_" (str (pii-time)))
			cmds_file (cat +rack_cmds run) out_file (cat +rack_out run) alive_file (cat +rack_alive run))
		(save (join cmds (ascii-char 10)) cmds_file)
		(save (str (pii-time)) alive_file)
		;a desktop of a run is hidden, it draws and is grabbed but is never
		;seen and never takes the input of whoever is at the machine, Chris:
		;"It can come up and down as much as it wants and it never steals my
		;input etc ? We get to work on the same desktop with no issues."
		(pii-spawn (cat (if gui "-gui -hidden " "") "-run (progn" tab "(def" tab "*root_env*" tab "'*rack_run*" tab "{" run "})"
			(if gui (cat tab "(def" tab "*root_env*" tab "'*rack_gui*" tab ":t)") "")
			(if user (cat tab "(def" tab "*root_env*" tab "'*env_node_user*" tab "{" user "})") "")
			;a run keeps to itself, the member is what is in the mesh
			tab "(def" tab "*root_env*" tab "'*env_node_solo*" tab ":t)"
			tab "(import" tab "{lib/rack/session.lisp}))"))
		;the session's last line is how long it took, "[12s]". The file is
		;there before it is all written, so it is that line that is waited
		;for, not the file: taken as soon as it was there, it could be empty.
		;The whole suite on the Pi was 105s to 145s; with every app opened
		;three times on a desktop of its own, tests -a, it is 310s to 370s,
		;past the 340s this was: under the 870s the asker waits for the reply
		(defq t0 (pii-time) res :nil done (cat "s]" (ascii-char 10)) told 0 beat 0
			asker_node (if asker (slice asker +mailbox_id_size -1)))
		(while (and (not (and (setq res (load out_file)) (ends-with done res)))
				(< (- (pii-time) t0) 840000000)
				;the one that asked is still there, or it is let go
				(or (not asker_node) (find asker_node (lisp-nodes))))
			;what is new of what it said, to the one that asked
			(when (and asker res (> (length res) told))
				(mail-send asker (cat "LINE " (slice res told -1)))
				(setq told (length res)))
			(when (> (- (pii-time) beat) 1000000)
				(save (str (setq beat (pii-time))) alive_file))
			(task-sleep 100000))
		(unless (and res (ends-with done res)) (setq res :nil))
		(each (# (if (pii-fstat %0) (pii-remove %0))) (list cmds_file out_file alive_file))
		(push acc (ifn res (cat "NO RESULT" (ascii-char 10)))))
		phases)
	(apply (const cat) (cat (list "") acc)))

(defun rack-run (cmdline &optional make_first leave_out gone user tree gui all skip only said)
	; (rack-run cmdline [make_first leave_out gone user tree gui all skip only said]) -> (line ...)
	;make every machine that takes a sync the same as this one, then run
	;the command line on each, and on this one. With make_first, make and
	;make all boot are run first, in a session before. leave_out is the
	;machines not to run on, as (sync-services) names them, they are still
	;made the same. gone is paths to remove on the others. tree is the one
	;that is sent, default the system's own, the test of this sends a small
	;one. With gui the session is a desktop, see rack-fresh. cmdline may be
	;several, with a ; between, run one after another. With all every line a
	;session said comes back, not only those of how it went. skip is the
	;machines not to touch at all, not made the same and not run on: one
	;that is not ours, in the mesh as a guest. only, if given, is the
	;machines to touch, the start of a system id each, and no other: our
	;own rig, with guests in the mesh of any kind. said, a function, is
	;given what each session says as it says it, so a run that hangs shows
	;where. The lines are SYNC and GONE
	;for each other machine, SKIP for each skipped, RAN for each machine,
	;and DONE
	(defq out (list) t0 (pii-time) me (hex-encode (system-id))
		rules_text (ifn (load (cat (setd tree ".") "/.gitignore")) "")
		reply_mbox (mail-mbox) asked 0 ran (list (list me (cat (cpu) "/" (os))))
		cmdlines (map (# (trim %0)) (split cmdline ";"))
		phases (if make_first (list (list "make" "make all boot") cmdlines) (list cmdlines)))
	(setd leave_out (list) gone (list) skip (list))
	(defq skipped? (lambda (id machine)
		(or (find machine skip)
			(and only (notany (# (starts-with (to-upper %0) id)) only)))))
	(each (lambda ((svc id machine root))
			(if (skipped? id machine) (push out (cat "SKIP " machine " " (slice id 0 8) " not made the same, not run"))))
		(filter (# (nql (second %0) me)) (sync-services)))
	(each (lambda ((svc id machine root))
		;the hashes of the system's tree are kept from run to run, in obj/
		(defq t1 (pii-time) result (sync-push svc tree rules_text :nil :nil
			(if (eql tree ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes")) (ends-with "/Windows" machine)))
		(push out (cond
			((not result) (cat "SYNC " machine " no answer"))
			((eql result :old) (setq result :nil) (cat "SYNC " machine " no answer, it has an older sync"))
			((eql result :refused) (setq result :nil) (cat "SYNC " machine " refused, it takes nothing from this machine, its sync_from"))
			(:t (cat "SYNC " machine " " (str (first result)) " sent " (str (second result)) " bytes "
				(str (elem-get result 3)) " failed " (str (/ (- (pii-time) t1) 1000)) "ms"))))
		;what was deleted here is removed there, those files and no others
		(when (and result (nempty? gone))
			(defq removed 0)
			(each (lambda (path)
				(sync-tell svc reply_mbox +sync_type_del path)
				(and (defq reply (sync-hear reply_mbox 5000000)) (= (first reply) 0)
					(setq removed (inc removed))))
				gone)
			(push out (cat "GONE " machine " " (str removed) " of " (str (length gone)) " removed")))
		(and result (= (elem-get result 3) 0) (push ran (list id machine))))
		(filter (# (and (nql (second %0) me) (not (skipped? (second %0) (third %0))))) (sync-services)))
	(each (lambda ((id machine))
		(defq nodes (if (eql id me) (list (task-nodeid)) (lisp-nodes (hex-decode id))))
		(cond
			((find machine leave_out) (push out (cat "RAN " machine " left out, not run")))
			((empty? nodes) (push out (cat "RAN " machine " no node of it is known")))
			(:t (open-task (str `(progn
					(import "lib/rack/rack.inc")
					(mail-send (hex-decode ,(hex-encode reply_mbox))
						(cat ,machine (ascii-char 10) (rack-fresh (quote ,phases) ,user ,gui
							(hex-decode ,(hex-encode reply_mbox)))))))
					(first nodes) +kn_call_pin 0 (mail-mbox))
				(setq asked (inc asked)))))
		ran)
	(defq replies (list) t2 (pii-time))
	;what sessions say as they go, to said, and what each came back with
	(while (and (< (length replies) asked) (< (- (pii-time) t2) 870000000))
		(cond
			((not (defq reply (mail-read-timeout reply_mbox 1000000))))
			((starts-with "LINE " reply) (if said (said (slice reply 5 -1))))
			((push replies reply))))
	(times (- asked (length replies)) (push out "RAN no reply"))
	(each (lambda (reply)
		(push out (cond
			(all (cat "RAN " reply))
			;the lines that say how it went, the rest is the session's chatter
			((cat "RAN " (join (filter (# (or (found? %0 "Modules") (found? %0 "Passed") (found? %0 "Failed")
					(found? %0 "FAIL") (found? %0 "rror") (found? %0 "boot_image") (found? %0 "/")
					(found? %0 "RESULT") (starts-with "[" %0)))
				(split reply (ascii-char 10))) " "))))))
		replies)
	(push out (cat "DONE " (str (/ (- (pii-time) t0) 1000)) "ms")))
