(report-header "Idle: how long a node has had nothing to run, the measure of how hard it works")

(defun id-idle () (elem-get (kernel-stats) 4))

(assert-eq "the kernel says five things of itself" 5 (length (kernel-stats)))
(assert-true "the last is the time it has been idle" (>= (id-idle) 0))

;a task that works and does not give way, the node is not idle at all
(defq id_before (id-idle) id_start (pii-time))
(while (< (- (pii-time) id_start) 50000))
(assert-eq "no idle time goes by while a task is flat out" id_before (id-idle))

;a task that sleeps. This is a solo module, it runs when the others are
;done, a node that has other tests at work on it is not idle. It is given
;a few goes all the same
(assert-true "idle time goes by while every task is asleep"
	(some (lambda (&)
		(defq before (id-idle))
		(task-sleep 100000)
		(> (id-idle) before)) (range 0 10)))
(defq id_before (id-idle) id_start (pii-time))
(task-sleep 50000)
(assert-true "and never more of it than there was time"
	(<= (- (id-idle) id_before) (- (pii-time) id_start)))
