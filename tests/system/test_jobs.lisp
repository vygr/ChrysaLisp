(report-header "System: jobs, a queue of work over a farm of children")

(import "lib/task/jobs.inc")

(structure +work +job_size
	(long num die))

(structure +work_reply +job_reply_size
	(long num))

(enums +jt 0
	(enum task reply timer))

(defq jt_select (list (mail-mbox) (mail-mbox) (mail-mbox))
	jobs (Jobs "tests/system/data/jobs_child.lisp"
		(elem-get jt_select +jt_task) (elem-get jt_select +jt_reply) 4))

(defun jt-work (num &optional die)
	(setf-> (str-alloc +work_size) (+work_num num) (+work_die (if die 1 0))))

(defun jt-run (work timeout)
	;add the work and take the answers till none are out, or too long
	;has gone by. The numbers that came back, sorted.
	(defq got (list) end (+ (pii-time) timeout) left :t)
	(. jobs :add work)
	(while (and left (< (pii-time) end))
		(mail-timeout (elem-get jt_select +jt_timer) 200000 0)
		(defq msg (mail-read (elem-get jt_select (defq idx (mail-select jt_select)))))
		(case idx
			(+jt_task (. jobs :launched msg))
			(+jt_reply
				(when (defq out (. jobs :answered msg))
					(push got (getf msg +work_reply_num))
					(if (= out 0) (setq left :nil))))
			(:t (. jobs :refresh 1000000))))
	(mail-timeout (elem-get jt_select +jt_timer) 0 0)
	(sort got (const -)))

(assert-eq "children" 4 (. jobs :size))
(assert-eq "nothing out" 0 (. jobs :out))
(assert-list-eq "every job answered, once" (map (# (* 2 %0)) (range 0 40))
	(jt-run (map (const jt-work) (range 0 40)) 10000000))
(assert-eq "none out after" 0 (. jobs :out))
(assert-list-eq "and again on the same children" '(200 202 204)
	(jt-run (map (const jt-work) '(100 101 102)) 10000000))

;every child started again, the queue emptied, and they work as before
(. jobs :restart)
(assert-eq "restart empties the queue" 0 (. jobs :out))
(assert-list-eq "after a restart" '(2 4 6 8) (jt-run (map (const jt-work) '(1 2 3 4)) 10000000))
(. jobs :close)
(assert-eq "close" 0 (. jobs :out))
