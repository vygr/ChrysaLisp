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
	(:t (defq nt_before (length (lisp-nodes)) nt_had (length (node-nets))
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
		(assert-eq "as many are noted as before" nt_had (length (node-nets)))))
