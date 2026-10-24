(report-header "Nets: a network of a shape added to a running one by name, and stopped as one")

;this changes the network, so it is in tests/solo/, and runs on its own

(defun nt-new (before &optional sids)
	; (nt-new before [sids]) -> (node ...)
	;the nodes of this machine, and of the networks of their own of these
	;system ids, that were not before: those the test made. Not a count of
	;all of them, a node of another test that goes meanwhile, or another
	;machine's, is nothing to it
	(filter (# (not (find %0 before))) (apply (const cat) (cat (list (lisp-nodes :t)) (map (const lisp-nodes) (ifn sids (list)))))))

(defun nt-seen (before n &optional sids)
	; (nt-seen before n [sids]) -> (node ...)
	;wait till n new nodes are seen, and they are
	(while (< (length (nt-new before sids)) n) (task-sleep 100000))
	(nt-new before sids))

(defun nt-unseen (nodes)
	; (nt-unseen nodes) -> :t
	;wait till none of these nodes is seen
	(while (some (# (find %0 (lisp-nodes))) nodes) (task-sleep 100000))
	:t)

(cond
	((eql (os) 'Windows) (test-skip "a network by name" "the note of one is not kept on Windows"))
	(:t ;a node that has started nodes before has a note already, one that
		;has not has none, and a network is found either way. The first
		;node of a session has, the test may be on any
		(defq nt_first (node-net :full 2 0 :nil :nil "tst0"))
		(defq nt_before (lisp-nodes :t) nt_had (length (node-nets))
			nt_pids (node-net :ring 4 0 :nil :nil "tst1"))
		(assert-eq "a ring of 4 with this node is 3 more" 3 (length nt_pids))
		(assert-true "each was started" (every (# (> %0 0)) nt_pids))
		(defq nt_mine (nt-seen nt_before 3))
		(assert-eq "they are seen" 3 (length nt_mine))
		(defq nt_net (some (# (if (eql (first %0) "tst1") %0)) (node-nets)))
		(assert-true "the network is noted by its name" nt_net)
		(assert-list-eq "with its shape and how many" '("ring" 4) (slice nt_net 1 3))
		(assert-list-eq "the processes of its nodes" (sort (cat nt_pids) (const -)) (sort (cat (elem-get nt_net 3)) (const -)))
		(assert-eq "and its links, a ring of 4 has 4" 4 (length (elem-get nt_net 4)))
		(assert-true "each link has its file" (every (# (pii-fstat (cat "/tmp/" %0))) (elem-get nt_net 4)))

		;a network that is a system of its own. It has a system id that is
		;not this machine's, this node is not one of it, and tasks are
		;not spread to it
		(defq nt_sid "tst2systemidabcd"
			nt_pids2 (node-net :ring 4 0 :nil :nil "tst2" nt_sid))
		(assert-eq "a ring of 4 of its own is 4 more" 4 (length nt_pids2))
		(assert-true "each was started" (every (# (> %0 0)) nt_pids2))
		(while (< (length (lisp-nodes nt_sid)) 4) (task-sleep 100000))
		(assert-eq "they are seen, with the system id they were given" 4 (length (lisp-nodes nt_sid)))
		(defq nt_theirs (lisp-nodes nt_sid))
		(assert-true "none of them is of this machine" (notany (# (find %0 (lisp-nodes :t))) nt_theirs))
		(defq nt_net2 (some (# (if (eql (first %0) "tst2") %0)) (node-nets)))
		(assert-list-eq "it is noted with its shape, how many, and its system id"
			(list "ring" 4 nt_sid) (list (second nt_net2) (third nt_net2) (last nt_net2)))
		(assert-eq "its links, the ring's 4 and the one from here" 5 (length (elem-get nt_net2 4)))
		(defq nt_mbox (mail-mbox) nt_mine (cat (system-id)) nt_away 0)
		(times 40 (open-task (str `(mail-send (hex-decode ,(hex-encode nt_mbox)) (cat (system-id))))
			(task-nodeid) +kn_call_run 0 (mail-mbox)))
		(times 40 (unless (eql (mail-read nt_mbox) nt_mine) (++ nt_away)))
		(assert-eq "a task left to find a node does not go to it" 0 nt_away)
		(open-task (str `(mail-send (hex-decode ,(hex-encode nt_mbox)) (cat (system-id))))
			(first (lisp-nodes nt_sid)) +kn_call_pin 0 (mail-mbox))
		(assert-eq "one sent to a node of it runs there" nt_sid (mail-read nt_mbox))

		;both are stopped now, after the tasks above were left to find a node.
		;A node that has gone is known of for a while yet, some 4 seconds,
		;and such a task can be sent to it and is lost
		(assert-eq "no network of another name" :nil (node-stop "nope"))
		(assert-eq "stopped, it says how many nodes are to go" 3 (node-stop "tst1"))
		(assert-eq "it is no longer noted" :nil (some (# (if (eql (first %0) "tst1") %0)) (node-nets)))
		(assert-true "its links are cleared away" (notany (# (pii-fstat (cat "/tmp/" %0))) (elem-get nt_net 4)))
		(while (some (const pii-alive) nt_pids) (task-sleep 100000))
		(assert-true "and its processes end" (notany (const pii-alive) nt_pids))
		(assert-eq "stopped, all 4 are to go" 4 (node-stop "tst2"))
		(while (some (const pii-alive) nt_pids2) (task-sleep 100000))
		(assert-true "and its processes end" (notany (const pii-alive) nt_pids2))
		(assert-eq "as many are noted as before" nt_had (length (node-nets)))
		(assert-true "and they are no longer seen" (and (nt-unseen nt_mine) (nt-unseen nt_theirs)))
		;networks hung from the nodes of another, as a terminal makes them,
		;each command placed on whichever node has least to do, and then all
		;of them stopped. A node that goes cuts off those behind it
		(import "lib/task/pipe.inc")
		(defq nt_quiet (lambda (&)) nt_was (map (const first) (node-nets)) nt_before (lisp-nodes :t))
		(pipe-run "nodes -s ring -n 4" nt_quiet)
		(times 3 (pipe-run "nodes -s ring -n 4 -o" nt_quiet))
		(defq nt_new (filter (# (not (find (first %0) nt_was))) (node-nets))
			nt_pids (reduce (# (cat %0 (elem-get %1 3))) nt_new (list)))
		(assert-eq "a ring and three of their own are four networks" 4 (length nt_new))
		(assert-eq "of 15 nodes" 15 (length nt_pids))
		(assert-eq "three have a system id" 3 (length (filter (const last) nt_new)))
		(assert-eq "they are all seen" 15
			(length (nt-seen nt_before 15 (filter (const identity) (map (const last) nt_new)))))
		(each (# (pipe-run (cat "nodes -x " (first %0)) nt_quiet)) nt_new)
		(while (some (const pii-alive) nt_pids) (task-sleep 100000))
		(assert-eq "stopped, not one is left" 0 (length (filter (const pii-alive) nt_pids)))
		(assert-eq "nor noted" 0 (length (filter (# (not (find (first %0) nt_was))) (node-nets))))

		(assert-eq "the one that was there all along is stopped last" 1 (node-stop "tst0"))
		;a stop is done a moment later, and the note of it is gone at once,
		;so a session that ended now would leave the node behind
		(while (some (const pii-alive) nt_first) (task-sleep 100000))
		(assert-true "and its process ends" (notany (const pii-alive) nt_first))))

;a network stopped by name is taken out of the note of the node that added
;it, and a note with nothing left in it goes. An empty one was left each time
(defq nn_note (cat "/tmp/chrysalisp_" (str (pii-pid)) ".session"))
(assert-true "the note of this node is gone, or has something in it"
	(or (= (age nn_note) 0) (nempty? (ifn (load nn_note) ""))))
