(report-header "Nets: a network of a shape added to a running one by name, and stopped as one")

;this changes the network, so it is in tests/solo/, and runs on its own

(defun nt-wait (want)
	;wait till that many nodes are seen, more or fewer than now, and not for ever
	(defq t0 (pii-time))
	(while (and (/= (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 15)))
		(task-sleep 100000))
	(length (lisp-nodes)))

(cond
	((eql (os) 'Windows) (test-skip "a network by name" "the note of one is not kept on Windows"))
	(:t ;a node that has started nodes before has a note already, one that
		;has not has none, and a network is found either way. The first
		;node of a session has, the test may be on any
		(defq nt_first (node-net :full 2 0 :nil :nil "tst0"))
		(defq nt_before (length (lisp-nodes)) nt_had (length (node-nets))
			nt_pids (node-net :ring 4 0 :nil :nil "tst1"))
		(assert-eq "a ring of 4 with this node is 3 more" 3 (length nt_pids))
		(assert-true "each was started" (every (# (> %0 0)) nt_pids))
		(assert-eq "they are seen" (+ nt_before 3) (nt-wait (+ nt_before 3)))
		(defq nt_net (some (# (if (eql (first %0) "tst1") %0)) (node-nets)))
		(assert-true "the network is noted by its name" nt_net)
		(assert-list-eq "with its shape and how many" '("ring" 4) (slice nt_net 1 3))
		(assert-list-eq "the processes of its nodes" (sort (cat nt_pids) (const -)) (sort (cat (elem-get nt_net 3)) (const -)))
		(assert-eq "and its links, a ring of 4 has 4" 4 (length (elem-get nt_net 4)))
		(assert-true "each link has its file" (every (# (pii-fstat (cat "/tmp/" %0))) (elem-get nt_net 4)))
		(assert-eq "no network of another name" :nil (node-stop "nope"))
		(assert-eq "stopped, it says how many nodes are to go" 3 (node-stop "tst1"))
		(assert-eq "it is no longer noted" :nil (some (# (if (eql (first %0) "tst1") %0)) (node-nets)))
		(assert-true "its links are cleared away" (notany (# (pii-fstat (cat "/tmp/" %0))) (elem-get nt_net 4)))
		(defq nt_t0 (pii-time))
		(while (and (some (const pii-alive) nt_pids) (< (- (pii-time) nt_t0) (task-timeout 10)))
			(task-sleep 100000))
		(assert-true "and its processes end" (notany (const pii-alive) nt_pids))
		(assert-eq "as many are noted as before" nt_had (length (node-nets)))
		;a node that has gone is known of for a while yet, and a task left
		;to find a node can be sent to it. Wait till it is not
		(assert-eq "and they are no longer seen" nt_before (nt-wait nt_before))

		;a network that is a system of its own. It has a system id that is
		;not this machine's, this node is not one of it, and tasks are
		;not spread to it
		(defq nt_sid "tst2systemidabcd" nt_same (length (lisp-nodes :t))
			nt_pids (node-net :ring 4 0 :nil :nil "tst2" nt_sid))
		(assert-eq "a ring of 4 of its own is 4 more" 4 (length nt_pids))
		(assert-true "each was started" (every (# (> %0 0)) nt_pids))
		(defq nt_t0 (pii-time))
		(while (and (< (length (lisp-nodes nt_sid)) 4) (< (- (pii-time) nt_t0) (task-timeout 15)))
			(task-sleep 100000))
		(assert-eq "they are seen, with the system id they were given" 4 (length (lisp-nodes nt_sid)))
		(assert-eq "this machine has the nodes it had" nt_same (length (lisp-nodes :t)))
		(defq nt_net (some (# (if (eql (first %0) "tst2") %0)) (node-nets)))
		(assert-list-eq "it is noted with its shape, how many, and its system id"
			(list "ring" 4 nt_sid) (list (second nt_net) (third nt_net) (last nt_net)))
		(assert-eq "its links, the ring's 4 and the one from here" 5 (length (elem-get nt_net 4)))
		(defq nt_mbox (mail-mbox) nt_mine (cat (system-id)) nt_away 0)
		(times 40 (open-task (str `(mail-send (hex-decode ,(hex-encode nt_mbox)) (cat (system-id))))
			(task-nodeid) +kn_call_run 0 (mail-mbox)))
		(times 40 (when (defq nt_reply (mail-read-timeout nt_mbox (task-timeout 5)))
			(unless (eql nt_reply nt_mine) (++ nt_away))))
		(assert-eq "a task left to find a node does not go to it" 0 nt_away)
		(open-task (str `(mail-send (hex-decode ,(hex-encode nt_mbox)) (cat (system-id))))
			(first (lisp-nodes nt_sid)) +kn_call_pin 0 (mail-mbox))
		(assert-eq "one sent to a node of it runs there" nt_sid (mail-read-timeout nt_mbox (task-timeout 5)))
		(assert-eq "stopped, all 4 are to go" 4 (node-stop "tst2"))
		(defq nt_t0 (pii-time))
		(while (and (some (const pii-alive) nt_pids) (< (- (pii-time) nt_t0) (task-timeout 10)))
			(task-sleep 100000))
		(assert-true "and its processes end" (notany (const pii-alive) nt_pids))
		(assert-eq "as many are noted as before" nt_had (length (node-nets)))
		;networks hung from the nodes of another, as a terminal makes them,
		;each command placed on whichever node has least to do, and then all
		;of them stopped. A node that goes cuts off those behind it
		(import "lib/task/pipe.inc")
		(defq nt_quiet (lambda (&)) nt_was (map (const first) (node-nets)))
		(pipe-run "nodes -s ring -n 8" nt_quiet)
		(times 3 (pipe-run "nodes -s ring -n 8 -o" nt_quiet))
		(defq nt_new (filter (# (not (find (first %0) nt_was))) (node-nets))
			nt_pids (reduce (# (cat %0 (elem-get %1 3))) nt_new (list)))
		(assert-eq "a ring and three of their own are four networks" 4 (length nt_new))
		(assert-eq "of 31 nodes" 31 (length nt_pids))
		(assert-eq "three have a system id" 3 (length (filter (const last) nt_new)))
		(task-sleep (task-timeout 1))
		(each (# (pipe-run (cat "nodes -x " (first %0)) nt_quiet)) nt_new)
		(defq nt_t0 (pii-time))
		(while (and (some (const pii-alive) nt_pids) (< (- (pii-time) nt_t0) (task-timeout 10)))
			(task-sleep 100000))
		(assert-eq "stopped, not one is left" 0 (length (filter (const pii-alive) nt_pids)))
		(assert-eq "nor noted" 0 (length (filter (# (not (find (first %0) nt_was))) (node-nets))))
		(assert-eq "they are no longer seen" nt_before (nt-wait nt_before))

		(assert-eq "the one that was there all along is stopped last" 1 (node-stop "tst0"))
		;a stop is done a moment later, and the note of it is gone at once,
		;so a session that ended now would leave the node behind
		(defq nt_t0 (pii-time))
		(while (and (some (const pii-alive) nt_first) (< (- (pii-time) nt_t0) (task-timeout 10)))
			(task-sleep 100000))
		(assert-true "and its process ends" (notany (const pii-alive) nt_first))))
