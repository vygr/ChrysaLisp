;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a gopher, a query sent to an object
;
; A task of its own. It is mailed who to answer, the Storage service that
; has the blocks, and then, as text: the path, the block to begin at, what
; it has, and a function. It goes to the blocks of the object one at a
; time. The function is given what it has, the block, which block it is
; and how many there are, and gives what it has now and the block to go to
; next, or :nil, it is done. Then what it has is sent straight to who
; asked, as text, in a list. If it can not go on, "none" is.
;
; What it has and the function are all there is of it, so it can be sent
; on, as it is now, to wherever the block it wants is: it is this file run
; again there.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "./app.inc")

(defun main ()
	(defq msg (mail-read (task-mbox)) lines (split msg +storage_lf)
		asker (hex-decode (first lines)) svc (hex-decode (second lines))
		;the rest is the one form, it may have line ends of its own in it
		text (slice msg (+ (length (first lines)) (length (second lines)) 2) -1)
		answer "none")
	(catch (progn
			(bind '(name at state source) (first (read (string-stream text))))
			;the function as it was sent is kept, to send on with what it
			;has then. What is run is a copy of it: to be run a form has
			;its macros done and its names bound, in the form itself,
			;Chris: "keep the original source so you can send it on with
			;the state", and one that had been would not read back
			(defq visit (exec (copy source)))
			(when (defq object (storage-manifest name svc))
				(defq names (second object) count (length names) lost :nil)
				(while (and at (not lost) (< -1 at count))
					(cond
						((defq data (storage-block (elem-get names at) svc))
							(bind '(state at) (visit state data at count)))
						((setq lost :t))))
				(unless lost (setq answer (str (list state))))))
		:t)
	(mail-send asker answer))
