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

(defun jt-run (work timeout &optional done patience)
	;add the work and take the answers till none are out, or done says
	;so, or too long has gone by. The numbers that came back, sorted.
	;patience is how long a child has for a job before it is taken to be
	;dead, and the job put back. It is long, a child that is only slow, on
	;a small machine with every test running at once, is not a dead one.
	;The run that has a job kill its child gives its own, short
	(defq got (list) end (+ (pii-time) timeout) left :t patience (ifn patience (task-timeout 5)))
	(. jobs :add work)
	(while (and left (< (pii-time) end) (not (and done (done))))
		(mail-timeout (elem-get jt_select +jt_timer) 200000 0)
		(defq msg (mail-read (elem-get jt_select (defq idx (mail-select jt_select)))))
		(case idx
			(+jt_task (. jobs :launched msg))
			(+jt_reply
				(when (defq out (. jobs :answered msg))
					(push got (getf msg +work_reply_num))
					(if (= out 0) (setq left :nil))))
			(:t (. jobs :refresh patience))))
	(mail-timeout (elem-get jt_select +jt_timer) 0 0)
	(sort got (const -)))

(assert-eq "children" 4 (. jobs :size))
(assert-eq "nothing out" 0 (. jobs :out))
(assert-list-eq "every job answered, once" (map (# (* 2 %0)) (range 0 40))
	(jt-run (map (const jt-work) (range 0 40)) (task-timeout 10)))
(assert-eq "none out after" 0 (. jobs :out))
(assert-list-eq "and again on the same children" '(200 202 204)
	(jt-run (map (const jt-work) '(100 101 102)) (task-timeout 10)))

;every child started again, the queue emptied, and they work as before
(. jobs :restart)
(assert-eq "restart empties the queue" 0 (. jobs :out))
(assert-list-eq "after a restart" '(2 4 6 8) (jt-run (map (const jt-work) '(1 2 3 4)) (task-timeout 10)))

;a job its child dies of is put back, again and again, and is counted,
;which is how a job no child can do is told from a slow one
(assert-eq "no job has been put back" 0 (. jobs :tries))
;it is run till the two that can be done are, and the other has been
;put back twice, however long a slow machine takes over that
(assert-list-eq "the jobs beside one that kills its child" '(10 12)
	(jt-run (list (jt-work 5) (jt-work 99 :t) (jt-work 6)) (task-timeout 30)
		(# (and (= (length got) 2) (>= (. jobs :tries) 2))) (task-timeout 1)))
(assert-eq "the one that kills is still out" 1 (. jobs :out))
(assert-true "and has been tried more than once" (>= (. jobs :tries) 2))
(. jobs :restart)
(assert-eq "a restart forgets the count" 0 (. jobs :tries))
(. jobs :close)
(assert-eq "close" 0 (. jobs :out))

;a herd, on the nodes of this machine, two and one more for each node
;past the first, and no more than three
(defq jobs (Jobs "tests/system/data/jobs_child.lisp"
	(elem-get jt_select +jt_task) (elem-get jt_select +jt_reply) '(3 2)))
(assert-eq "a herd" (min 3 (inc (length (lisp-nodes :t)))) (. jobs :size))
(assert-list-eq "a herd does the work" '(2 4 6) (jt-run (map (const jt-work) '(1 2 3)) (task-timeout 10)))
(. jobs :close)

;children kept away from this node, if this machine has another. New
;mailboxes, so that no word of a child of the farms before is taken for
;one of these
(defq jt_select (list (mail-mbox) (mail-mbox) (mail-mbox)))
(defq jobs (Jobs "tests/system/data/jobs_child.lisp"
	(elem-get jt_select +jt_task) (elem-get jt_select +jt_reply) '(3 3 0) :t)
	jt_nodes (list))
(mail-timeout (elem-get jt_select +jt_timer) (task-timeout 5) 0)
(while (and (< (length jt_nodes) 3)
		(= (defq jt_idx (mail-select jt_select)) +jt_task))
	(defq jt_msg (mail-read (elem-get jt_select jt_idx)))
	(push jt_nodes (task-nodeid (getf jt_msg +kn_msg_reply_id)))
	(. jobs :launched jt_msg))
(mail-timeout (elem-get jt_select +jt_timer) 0 0)
(assert-eq "three children kept away" 3 (length jt_nodes))
(assert-true "none of them on this node, if there is another"
	(or (<= (length (lisp-nodes :t)) 1) (notany (# (eql %0 (task-nodeid))) jt_nodes)))
(assert-list-eq "and they work" '(2 4 6) (jt-run (map (const jt-work) '(1 2 3)) (task-timeout 10)))
(. jobs :close)
