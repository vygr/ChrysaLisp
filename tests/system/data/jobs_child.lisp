;a child for the test of lib/task/jobs.inc, it answers a job with its
;number doubled
(import "lib/task/jobs.inc")

(structure +work +job_size
	(long num die))

(structure +work_reply +job_reply_size
	(long num))

(defun main ()
	(defq running :t)
	(while running
		(defq msg (mail-read (task-mbox)))
		(cond
			((eql msg "") (setq running :nil))
			((/= (getf msg +work_die) 0) (setq running :nil))
			(:t (mail-send (getf msg +job_reply)
				(setf-> (str-alloc +work_reply_size)
					(+job_reply_key (getf msg +job_key))
					(+work_reply_num (* 2 (getf msg +work_num)))))))))
