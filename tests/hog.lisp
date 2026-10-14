;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; a busy app, for a test of the bands: a millisecond or two of work, then it
; gives way, till it is mailed. With a mailbox in hex it is a probe instead:
; it asks to be woken every 10ms, and says there, when it is mailed again,
; the longest it was kept waiting past that, us
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	(defq what (mail-read (task-mbox)) n 0 worst 0)
	(cond
		((eql what "busy")
			(until (mail-poll (list (task-mbox)))
				(defq i 0)
				(while (< (++ i) 20000) (setq n (+ n i)))
				(task-slice)))
		(:t (defq reply (hex-decode what))
			(until (mail-poll (list (task-mbox)))
				(defq asked (pii-time))
				(task-sleep 10000)
				(setq worst (max worst (- (pii-time) asked 10000))))
			(mail-send reply (str worst)))))
