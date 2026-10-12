;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a gopher, a query sent to an object
;
; A task of its own. It is mailed who to answer, the Storage service that
; has the blocks, and then, as text: the path, the block to begin at, what
; it has, a function, where it has been and the members that did not
; answer it. It goes to the blocks of the object one at a
; time. The function is given what it has, the block, which block it is
; and how many there are, and gives what it has now and the block to go to
; next, or :nil, it is done. Then what it has is sent straight to who
; asked, as text, in a list. If it can not go on, "none" is.
;
; What it has and the function are all there is of it, so it can be sent
; on, as it is now, to wherever the block it wants is: it is this file run
; again there. How it is sent on, and what is done when that fails, is in
; ./gopher.inc.
;
; One that is started and is sent nothing, in a while, goes. One that is
; sent on says so, to the one that sent it, before it does anything else.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")
(import "./gopher.inc")

(defun main ()
	;all there is of it comes in one mail. None in a while, it was started
	;and whoever started it is gone, or gave it up: so does this
	(when (defq msg (mail-read-timeout (task-mbox) (task-timeout +gopher_state_wait)))
		(defq lines (split msg +storage_lf)
			asker (hex-decode (first lines)) svc (hex-decode (second lines)) ack (third lines)
			;the rest is the one form, it may have line ends of its own in it
			text (slice msg (+ (length (first lines)) (length (second lines)) (length ack) 3) -1)
			answer "none")
		;it is here, with all of itself: the one that sent it on can go
		(if (nql ack "-") (mail-send (hex-decode ack) "ok"))
		(catch (progn
				(bind '(name at state source trail skip) (first (read (string-stream text))))
				;the function as it was sent is kept, to send on with what it
				;has then. What is run is a copy of it: to be run a form has
				;its macros done and its names bound, in the form itself,
				;Chris: "keep the original source so you can send it on with
				;the state", and one that had been would not read back
				(defq visit (exec (copy source)) me (storage-state svc))
				;where it has been: each member, and the block it came to it for
				(push trail (list (if me (. me :find :member) "?") at))
				(when (defq object (storage-manifest name svc))
					(defq names (second object) count (length names) lost :nil sent :nil)
					(while (and at (not lost) (not sent) (< -1 at count))
						(defq want (elem-get names at))
						(cond
							((defq data (storage-block-at want svc))
								(bind '(state at) (visit state data at count)))
							;the block is not kept here. The gopher goes to it: all
							;there is of it, where it is, what it has, its function
							;and where it has been, is sent to a member that has the
							;block, to carry on there. This one is then done and says
							;nothing. If none of them takes it, it can not go on
							((gopher-pass (gopher-order names at (storage-where want svc) skip) asker name at state source trail skip)
								(setq sent :t))
							((setq lost :t))))
					(cond
						(sent (setq answer :nil))
						((not lost) (setq answer (str (list state trail)))))))
			:t)
		(if answer (mail-send asker answer))))
