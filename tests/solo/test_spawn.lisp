(report-header "Spawn: a node starts another, runs a task on it, and ends it")

;this changes the network, so it is in tests/solo/, and runs on its own

(defq ht_before (lisp-nodes) ht_pid (first (node-spawn))
	ht_t0 (pii-time) ht_new :nil ht_reply (mail-mbox))

(defun ht-find ()
	;the node that is the process started, :nil if not seen yet. A node
	;not seen before need not be it, one may have dropped out and come back.
	(some (lambda (node)
			(unless (find node ht_before)
				(open-task (str `(mail-send (hex-decode ,(hex-encode ht_reply)) (str (pii-pid))))
					node +kn_call_pin 0 (mail-mbox))
				(if (eql (mail-read-timeout ht_reply (task-timeout 1)) (str ht_pid)) node)))
		(lisp-nodes)))

(assert-true "a node is started" (> ht_pid 0))
(assert-eq "its process is running" :t (pii-alive ht_pid))
(while (and (not (setq ht_new (ht-find))) (< (- (pii-time) ht_t0) (task-timeout 5)))
	(task-sleep 10000))
(assert-true "it is seen on the network, and runs a task" ht_new)
(when ht_new
	(open-task "(pii-exit)" ht_new +kn_call_pin 0 (mail-mbox))
	(setq ht_t0 (pii-time))
	(while (and (pii-alive ht_pid) (< (- (pii-time) ht_t0) (task-timeout 5)))
		(task-sleep 10000))
	(assert-eq "told to exit, its process ends" :nil (pii-alive ht_pid))
	;a link sees its peer's process is gone, and the node is forgotten at
	;once, it is not left to run out its time
	(setq ht_t0 (pii-time))
	(while (and (find ht_new (lisp-nodes)) (< (- (pii-time) ht_t0) (task-timeout 3)))
		(task-sleep 10000))
	(assert-true "and it is gone from the network" (not (find ht_new (lisp-nodes)))))
