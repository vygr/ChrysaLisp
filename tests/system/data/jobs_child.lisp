;a child for the test of lib/task/jobs.inc, it answers a job with its
;number doubled, after the wait it is given, in us
(import "lib/task/jobs.inc")

(structure +work +job_size
	(long num die wait))

(structure +work_reply +job_reply_size
	(long num))

(defun main ()
	(defq running :t)
	(while running
		(defq msg (mail-read-timeout (task-mbox) 10000000))
		(cond
			((or (not msg) (eql msg "")) (setq running :nil))
			((/= (getf msg +work_die) 0) (setq running :nil))
			(:t (if (> (getf msg +work_wait) 0) (task-sleep (getf msg +work_wait)))
				(mail-send (getf msg +job_reply)
					(setf-> (str-alloc +work_reply_size)
						(+job_reply_key (getf msg +job_key))
						(+work_reply_num (* 2 (getf msg +work_num)))))))))
