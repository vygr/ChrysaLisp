;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Storage, what the app watches: a scene run, or the store of this system
;
; A task of its own, started by the Storage app, so the app goes on drawing
; while a scene runs. It is mailed who to tell and what to watch, a line
; each: the name of a scene of tests/storage/, or - for the store this
; system is a member of. It tells, in batches, "lines" and then a line of a
; recording each, service/storage/app.inc, what the scene says among them
; as "time - say text"; and at the end "end", with how many of the scene's
; checks held and how many did not. The store of this system is watched
; till it is told to stop, a mail of anything.
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(import "service/storage/scene.inc")

;how long lines are kept before they are sent, and how many at most
(defq +run_hold 100000 +run_most 400 +run_lf (ascii-char 10))

(defun main ()
	(bind '(to what) (split (mail-read (task-mbox)) +run_lf))
	(defq to (hex-decode to) pending (list) sent (pii-time))
	(defun pass (line)
		;a line kept, and what is kept sent when it has waited or is many,
		;or nothing more has come for a while, :nil
		(if line (push pending line))
		(when (and (nempty? pending) (or (not line) (>= (length pending) +run_most) (> (- (pii-time) sent) +run_hold)))
			(mail-send to (cat "lines" +run_lf (join pending +run_lf)))
			(setq pending (list) sent (pii-time))))
	(defq result (if (eql what "-")
			(progn (storage-record-run pass 0x7fffffffffffffff :nil (task-mbox)) :nil)
			(scene-run what (lambda (text) (pass (cat (str (pii-time)) " - say " text))) pass)))
	(if (nempty? pending) (mail-send to (cat "lines" +run_lf (join pending +run_lf))))
	(mail-send to (cat "end " (if result (join (map (const str) result) " ") "- -"))))
