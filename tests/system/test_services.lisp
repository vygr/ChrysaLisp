(report-header "Services: a * service declared on another node is seen here, and seen to go")

;the routing ping holds only a hash of a node's services. A node that does
;not hold the services for that hash asks, and gets them in a full ping.

(defq sv_nodes (net-quiet 300000 6) sv_me (task-nodeid)
	sv_far (filter (# (nql %0 sv_me)) sv_nodes))

(defun sv-wait (want)
	;wait for the test service to be seen, or to be gone, :t if it was
	(defq t0 (pii-time) ok :nil)
	(while (and (not ok) (< (- (pii-time) t0) 4000000))
		(if (eql want (nempty? (mail-enquire "*ServiceEdge"))) (setq ok :t)
			(task-sleep 10000)))
	ok)

(cond
	((empty? sv_far)
		(test-skip "a service on another node" "needs more than one node"))
	(:t (defq sv_reply (mail-mbox) sv_code (str `(progn
			(defq key (mail-declare (task-mbox) "*ServiceEdge" "a test"))
			(mail-send (hex-decode ,(hex-encode sv_reply)) "declared")
			(mail-read (task-mbox))
			(mail-forget key)
			(mail-send (hex-decode ,(hex-encode sv_reply)) "forgotten")
			(task-sleep 200000))))
		(assert-eq "not there to start with" 0 (length (mail-enquire "*ServiceEdge")))
		(open-task sv_code (last sv_far) +kn_call_pin 0 (defq sv_task (mail-mbox)))
		(assert-eq "the far task declares it" "declared" (mail-read-timeout sv_reply 3000000))
		(assert-eq "and it is seen here" :t (sv-wait :t))
		(defq sv_entry (first (mail-enquire "*ServiceEdge")))
		(assert-eq "with its info" "a test" (last (split sv_entry ",")))
		;its mailbox is in the entry, tell it to forget the service
		(mail-send (hex-decode (second (split sv_entry ","))) "go")
		(assert-eq "the far task forgets it" "forgotten" (mail-read-timeout sv_reply 3000000))
		(assert-eq "and it is gone here" :t (sv-wait :nil))))
