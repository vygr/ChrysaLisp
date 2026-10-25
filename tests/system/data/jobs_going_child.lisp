;a child for the test of lib/task/jobs.inc that goes of its own accord, by
;(job-child), when it has had nothing for two seconds: long enough that a
;busy machine gives it its first job before it goes. It answers a job with
;its number doubled
(import "lib/task/jobs.inc")

(structure +work +job_size
	(long num die))

(structure +work_reply +job_reply_size
	(long num))

(defun main ()
	(job-child 2000000 (lambda (msg mbox)
		(mail-send (getf msg +job_reply)
			(setf-> (str-alloc +work_reply_size)
				(+job_reply_key (getf msg +job_key))
				(+work_reply_num (* 2 (getf msg +work_num))))))))
