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
(defq +rack_cmds "/tmp/chrysalisp_rack_cmds" +rack_out "/tmp/chrysalisp_rack_out")

(defun rack-fresh (phases &optional user gui)
	; (rack-fresh phases [user gui]) -> str
	;on this machine, a new session for each phase, a list of command
	;lines, one after another, and what they said. A session is started by
	;the host, so it is on the boot image as it is on disk now, which a
	;phase before it may have made. With a user the session is theirs, and
	;not whoever last signed on to the machine, as run.sh -u has it. With
	;gui the session is on the GUI host, a desktop, its GUI up before the
	;command lines are run, as run.sh -f has it
	;the form has no space or tab in it, a node's args are split at those
	(defq acc (list) tab (ascii-char 11))
	(each (lambda (cmds)
		;the name of this run, the session is told it as it is told its user
		(defq run (cat "_" (str (pii-pid)) "_" (str (pii-time)))
			cmds_file (cat +rack_cmds run) out_file (cat +rack_out run))
		(save (join cmds (ascii-char 10)) cmds_file)
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
		;The whole suite on the Pi is 105s to 145s, and was 192s twice with
		;the other two running theirs, past the 170s this was: under the
		;360s the asker waits for the reply
		(defq t0 (pii-time) res :nil done (cat "s]" (ascii-char 10)))
		(while (and (not (and (setq res (load out_file)) (ends-with done res)))
				(< (- (pii-time) t0) 340000000))
			(task-sleep 100000))
		(unless (and res (ends-with done res)) (setq res :nil))
		(if (pii-fstat cmds_file) (pii-remove cmds_file))
		(if (pii-fstat out_file) (pii-remove out_file))
		(push acc (ifn res (cat "NO RESULT" (ascii-char 10)))))
		phases)
	(apply (const cat) (cat (list "") acc)))

(defun rack-run (cmdline &optional make_first leave_out gone user tree gui all)
	; (rack-run cmdline [make_first leave_out gone user tree gui all]) -> (line ...)
	;make every machine that takes a sync the same as this one, then run
	;the command line on each, and on this one. With make_first, make and
	;make all boot are run first, in a session before. leave_out is the
	;machines not to run on, as (sync-services) names them, they are still
	;made the same. gone is paths to remove on the others. tree is the one
	;that is sent, default the system's own, the test of this sends a small
	;one. With gui the session is a desktop, see rack-fresh. cmdline may be
	;several, with a ; between, run one after another. With all every line a
	;session said comes back, not only those of how it went. The lines are SYNC and GONE for each other machine, RAN for each
	;machine, and DONE
	(defq out (list) t0 (pii-time) me (hex-encode (system-id))
		rules_text (ifn (load (cat (setd tree ".") "/.gitignore")) "")
		reply_mbox (mail-mbox) asked 0 ran (list (list me (cat (cpu) "/" (os))))
		cmdlines (map (# (trim %0)) (split cmdline ";"))
		phases (if make_first (list (list "make" "make all boot") cmdlines) (list cmdlines)))
	(setd leave_out (list) gone (list))
	(each (lambda ((svc id machine root))
		;the hashes of the system's tree are kept from run to run, in obj/
		(defq t1 (pii-time) result (sync-push svc tree rules_text :nil :nil
			(if (eql tree ".") (cat "obj/" (cpu) "/" (abi) "/sync_hashes")) (ends-with "/Windows" machine)))
		(push out (cond
			((not result) (cat "SYNC " machine " no answer"))
			((eql result :old) (setq result :nil) (cat "SYNC " machine " no answer, it has an older sync"))
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
		(filter (# (nql (second %0) me)) (sync-services)))
	(each (lambda ((id machine))
		(defq nodes (if (eql id me) (list (task-nodeid)) (lisp-nodes (hex-decode id))))
		(cond
			((find machine leave_out) (push out (cat "RAN " machine " left out, not run")))
			((empty? nodes) (push out (cat "RAN " machine " no node of it is known")))
			(:t (open-task (str `(progn
					(import "lib/rack/rack.inc")
					(mail-send (hex-decode ,(hex-encode reply_mbox))
						(cat ,machine (ascii-char 10) (rack-fresh (quote ,phases) ,user ,gui)))))
					(first nodes) +kn_call_pin 0 (mail-mbox))
				(setq asked (inc asked)))))
		ran)
	(times asked
		(push out (cond
			((not (defq reply (mail-read-timeout reply_mbox 360000000))) "RAN no reply")
			(all (cat "RAN " reply))
			;the lines that say how it went, the rest is the session's chatter
			((cat "RAN " (join (filter (# (or (found? %0 "Modules") (found? %0 "Passed") (found? %0 "Failed")
					(found? %0 "FAIL") (found? %0 "rror") (found? %0 "boot_image") (found? %0 "/")
					(found? %0 "RESULT") (starts-with "[" %0)))
				(split reply (ascii-char 10))) " "))))))
	(push out (cat "DONE " (str (/ (- (pii-time) t0) 1000)) "ms")))
