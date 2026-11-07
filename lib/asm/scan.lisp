;;;;;;;;;;;;;;;;;;;;
; Source Scan Child
;;;;;;;;;;;;;;;;;;;;

(import "lib/task/jobs.inc")

;a worker with no job for this long ends, its farm has gone and did not
;say. A job sent to it as it went is given to another, (job-child), but
;the farm's node may be busy, and it is long so that is seldom.
(defq +timeout (task-timeout 60))

(defun main ()
	(job-child +timeout (lambda (msg mbox)
		;the files follow the job's head, lib/task/jobs.inc, and the reply is
		;the key it was given, then each file with its (files-scan), for
		;make, lib/asm/asm.inc (scans-farm)
		(defq reply_key (getf msg +job_key) reply_mbox (getf msg +job_reply)
			files (first (read (string-stream (slice msg +job_size -1)))))
		(mail-send reply_mbox (cat (char reply_key +long_size)
			(str (map (# (list %0 (files-scan %0))) files)))))))
