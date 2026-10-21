;;;;;;;;;;;
; Cmd Child
;;;;;;;;;;;

(import "./pipe.inc")
(import "./jobs.inc")

;a worker with no job for this long ends, its farm has gone and did not
;say. A job sent to it as it went is given to another, (job-child), but
;the farm's node may be busy, and it is long so that is seldom.
(defq +timeout (task-timeout 60))

(defun main ()
	(job-child +timeout (lambda (msg mbox)
		;what to run follows the job's head, lib/task/jobs.inc, and the
		;reply is the key it was given, then what the command said
		(defq reply_key (getf msg +job_key) reply_mbox (getf msg +job_reply)
			cmd (slice msg +job_size -1) out (string-stream (cat "")))
		(write-blk out (char reply_key +long_size))
		(catch (pipe-run cmd (# (write-blk out (str %0))))
			(progn (write-line out (str _ " " cmd)) :t))
		(mail-send reply_mbox (str out)))))
