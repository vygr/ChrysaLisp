(report-header "User: a session can be some user's, and not whoever last signed on to the machine")

;this starts nodes, so it is in tests/solo/, and runs on its own

;Who the user is was one file for the machine, usr/current, and so the
;same for every session on it. A session started as a user's, run.sh -u
;name, has *env_node_user* in the environment every task of a node shares,
;usr/env.inc looks there first, and (node-start) tells each node it starts.
;Here a node is started as if this one were a user's, and asked.

(defun us-ask (node)
	;who a task on that node finds the user to be, as an app does
	(defq mbox (mail-mbox))
	(open-task (str `(progn (import "usr/env.inc") (mail-send (hex-decode ,(hex-encode mbox)) (cat *env_user* " " *env_home*))))
		node +kn_call_pin 0 (mail-mbox))
	(mail-read-timeout mbox (task-timeout 5)))

(defun us-new (before)
	;wait for a node that was not there before, and not for ever
	(defq t0 (pii-time) node :nil)
	(until (or (setq node (some (# (unless (find %0 before) %0)) (lisp-nodes)))
			(> (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 100000))
	node)

(defq us_mine (us-ask (task-nodeid)) us_before (lisp-nodes))
;as if this node had been told it was Test's
(defq *env_node_user* "Test")
(defq us_pids (node-spawn 1) us_node (us-new us_before))
(assert-true "a node is started" (and us_node (every (# (> %0 0)) us_pids)))
(assert-eq "a task on it finds the user it was told, and their home" "Test usr/Test/" (us-ask us_node))
(assert-eq "a task on this node has the user it had" us_mine (us-ask (task-nodeid)))
;what that node starts is told as well, it has it from the node that started it
(defq us_mbox (mail-mbox) us_before (lisp-nodes))
(open-task (str `(mail-send (hex-decode ,(hex-encode us_mbox)) (str (node-spawn 1))))
	us_node +kn_call_pin 0 (mail-mbox))
(defq us_more (first (read (string-stream (ifn (mail-read-timeout us_mbox (task-timeout 10)) "()"))))
	us_node2 (us-new us_before))
(assert-true "it starts a node of its own" (and us_node2 (nempty? us_more)))
(assert-eq "and that one has the user too" "Test usr/Test/" (us-ask us_node2))
;with that taken away again a node is told what this one really has, the
;user of its session if it is some user's, as a test run by rack -u is,
;and nothing if not, when it has the machine's as this one does
(undef (env) '*env_node_user*)
(defq us_before (lisp-nodes) us_pids2 (node-spawn 1) us_node3 (us-new us_before))
(assert-eq "a node this one starts has the user this one has" us_mine (us-ask us_node3))
;clear them away
(each (# (if %0 (open-task "(pii-exit)" %0 +kn_call_pin 0 (mail-mbox)))) (list us_node2 us_node us_node3))
(defq us_all (cat us_pids us_more us_pids2) us_t0 (pii-time))
(while (and (some (const pii-alive) us_all) (< (- (pii-time) us_t0) (task-timeout 10)))
	(task-sleep 100000))
(assert-true "and they go" (notany (const pii-alive) us_all))
