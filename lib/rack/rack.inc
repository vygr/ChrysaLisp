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
;in. One for a machine, so one rack run on a machine at a time
(defq +rack_cmds "/tmp/chrysalisp_rack_cmds" +rack_out "/tmp/chrysalisp_rack_out")

(defun rack-fresh (phases)
	; (rack-fresh phases) -> str
	;on this machine, a new session for each phase, a list of command
	;lines, one after another, and what they said. A session is started by
	;the host, so it is on the boot image as it is on disk now, which a
	;phase before it may have made
	(defq acc (list))
	(each (lambda (cmds)
		(if (pii-fstat +rack_out) (pii-remove +rack_out))
		(save (join cmds (ascii-char 10)) +rack_cmds)
		(pii-spawn "-run lib/rack/session.lisp")
		(defq t0 (pii-time) res :nil)
		(while (and (not (setq res (load +rack_out))) (< (- (pii-time) t0) 170000000))
			(task-sleep 100000))
		(push acc (ifn res (cat "NO RESULT" (ascii-char 10)))))
		phases)
	(apply (const cat) (cat (list "") acc)))

(defun rack-run (cmdline &optional make_first leave_out gone)
	; (rack-run cmdline [make_first leave_out gone]) -> (line ...)
	;make every machine that takes a sync the same as this one, then run
	;the command line on each, and on this one. With make_first, make and
	;make all boot are run first, in a session before. leave_out is the
	;machines not to run on, as (sync-services) names them, they are still
	;made the same. gone is paths to remove on the others. The lines are
	;SYNC and GONE for each other machine, RAN for each machine, and DONE
	(defq out (list) t0 (pii-time) me (hex-encode (system-id))
		rules_text (ifn (load ".gitignore") "")
		reply_mbox (mail-mbox) asked 0 ran (list (list me (cat (cpu) "/" (os))))
		phases (if make_first (list (list "make" "make all boot") (list cmdline)) (list (list cmdline))))
	(setd leave_out (list) gone (list))
	(each (lambda ((svc id machine root))
		(defq t1 (pii-time) result (sync-push svc "." rules_text :nil :nil
			(cat "obj/" (cpu) "/" (abi) "/sync_hashes") (ends-with "/Windows" machine)))
		(push out (if result
			(cat "SYNC " machine " " (str (first result)) " sent " (str (second result)) " bytes "
				(str (elem-get result 3)) " failed " (str (/ (- (pii-time) t1) 1000)) "ms")
			(cat "SYNC " machine " no answer")))
		;what was deleted here is removed there, those files and no others
		(when (and result (nempty? gone))
			(defq removed 0)
			(each (lambda (path)
				(sync-tell svc reply_mbox +sync_type_del path)
				(if (and (defq reply (sync-hear reply_mbox 5000000)) (= (first reply) 0))
					(setq removed (inc removed))))
				gone)
			(push out (cat "GONE " machine " " (str removed) " of " (str (length gone)) " removed")))
		(if (and result (= (elem-get result 3) 0)) (push ran (list id machine))))
		(filter (# (nql (second %0) me)) (sync-services)))
	(each (lambda ((id machine))
		(defq nodes (if (eql id me) (list (task-nodeid)) (lisp-nodes (hex-decode id))))
		(cond
			((find machine leave_out) (push out (cat "RAN " machine " left out, not run")))
			((empty? nodes) (push out (cat "RAN " machine " no node of it is known")))
			(:t (open-task (str `(progn
					(import "lib/rack/rack.inc")
					(mail-send (hex-decode ,(hex-encode reply_mbox))
						(cat ,machine (ascii-char 10) (rack-fresh (quote ,phases))))))
					(first nodes) +kn_call_pin 0 (mail-mbox))
				(setq asked (inc asked)))))
		ran)
	(times asked
		(push out (if (defq reply (mail-read-timeout reply_mbox 360000000))
			;the lines that say how it went, the rest is the session's chatter
			(cat "RAN " (join (filter (# (or (found? %0 "Modules") (found? %0 "Passed") (found? %0 "Failed")
					(found? %0 "FAIL") (found? %0 "rror") (found? %0 "boot_image") (found? %0 "/")
					(found? %0 "RESULT") (starts-with "[" %0)))
				(split reply (ascii-char 10))) " "))
			"RAN no reply")))
	(push out (cat "DONE " (str (/ (- (pii-time) t0) 1000)) "ms")))
