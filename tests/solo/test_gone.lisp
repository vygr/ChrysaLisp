(report-header "Gone: a node that has just gone is not sent a task, and what was on its link is sent another way")

;this starts and stops nodes, so it is in tests/solo/, and runs on its own

;A link learns that its peer's process has gone at its next beat, a second.
;Till then the kernel took the node for one with little to do, and a task
;left to find a node, +kn_call_run, went to it and was lost, with whoever
;waited to hear of it. And mail that was on the link for it to pass on went
;with it. The kernel now asks the host if the process of the node it has
;chosen is there, and the link, when it finds it gone, takes back what was
;never taken and posts it again, sys/kernel/class.vp and sys/link/class.vp.

(defun gn-seen (want)
	;wait till that many nodes are seen, and not for ever
	(defq t0 (pii-time))
	(while (and (< (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 50000)))

(defun gn-gone (pids)
	;wait till these processes have gone, and not for ever
	(defq t0 (pii-time))
	(while (and (some (const pii-alive) pids) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 2000))
	(notany (const pii-alive) pids))

;a node with one link to here starts six more, they are stopped, and at
;once it leaves tasks to find a node. The six that have gone are the
;neighbours it knows with the least to do, tests/solo/gone_node.lisp. It
;is told what the tasks are by mail, they have this node's mailbox in them
(defq gn_were (length (lisp-nodes)) gn_before (lisp-nodes) gn_pid (node-spawn 1) gn_mbox (mail-mbox) gn_done (mail-mbox) gn_kn (mail-mbox))
(gn-seen (inc (length gn_before)))
(defq gn_node (some (# (unless (find %0 gn_before) %0)) (lisp-nodes)))
(assert-true "a node is started" gn_node)
(open-task "tests/solo/gone_node.lisp" gn_node +kn_call_pin 0 gn_kn)
(mail-send (getf (mail-read-timeout gn_kn (task-timeout 5)) +kn_msg_reply_id)
	(cat gn_done (str `(mail-send (hex-decode ,(hex-encode gn_mbox)) "x"))))
(assert-eq "it starts six, and stops them" "6" (mail-read-timeout gn_done (task-timeout 15)))
(defq gn_got 0)
(while (and (< gn_got 60) (mail-read-timeout gn_mbox (task-timeout 2))) (++ gn_got))
(assert-eq "of 60 tasks it left to find a node at once after, every one runs" 60 gn_got)
(open-task "(pii-exit)" gn_node +kn_call_pin 0 (mail-mbox))
(assert-true "and it is stopped" (gn-gone gn_pid))

;a ring of 4, this node and three more, so there are two ways round to the
;node across it. One of the three stops taking from its links, as a busy
;node does, and then goes. Mail sent meanwhile to the other two, some of it
;by way of that one
(defq gn_before (lisp-nodes) gn_pids (node-net :ring 4 0 :nil :nil "gn_r"))
(gn-seen (+ (length gn_before) 3))
(defq gn_ring (filter (# (not (find %0 gn_before))) (lisp-nodes))
	gn_victim (first gn_ring) gn_others (rest gn_ring))
(assert-eq "a ring of 4 is three more" 3 (length gn_ring))
;the ways round are worked out from the pings, give them a moment
(task-sleep 800000)
(defq gn_counters (map (lambda (node)
	(open-task (str `(progn (defq n 0 back (hex-decode ,(hex-encode gn_mbox)))
			(mail-send back (task-mbox))
			(while (nql (mail-read (task-mbox)) "report") (++ n))
			(mail-send back (str n))))
		node +kn_call_pin 0 (mail-mbox))
	(mail-read-timeout gn_mbox (task-timeout 5))) gn_others))
(assert-true "a counter on each of the other two" (every (const identity) gn_counters))
(open-task "(progn (defq t0 (pii-time)) (while (< (- (pii-time) t0) 800000)) (pii-exit))"
	gn_victim +kn_call_pin 0 (mail-mbox))
(task-sleep 200000)
(times 250 (each (# (mail-send %0 "x")) gn_counters))
;it goes, its links find that it has within a beat, and what was on them is posted again
(task-sleep (+ 800000 1000000 700000))
(each (# (mail-send %0 "report")) gn_counters)
(assert-list-eq "of 250 sent to each of the two, all arrive, what went by way of the one that has gone too"
	'("250" "250") (map (lambda (&) (ifn (mail-read-timeout gn_mbox (task-timeout 5)) "none")) gn_counters))
;each is told to go, by node. A network is stopped by name from the note
;of it, (node-stop), and no note is kept on Windows
(each (# (open-task "(pii-exit)" %0 +kn_call_pin 0 (mail-mbox))) gn_others)
(node-stop "gn_r")
(assert-true "the ring is stopped" (gn-gone gn_pids))
;a node that has gone is no longer listed by the node that had a link to
;it, nor are those that were only reached by way of it, as these were.
;The test that runs after this one may count the nodes
(defq gn_t0 (pii-time))
(while (and (> (length (lisp-nodes)) gn_were) (< (- (pii-time) gn_t0) (task-timeout 15)))
	(task-sleep 100000))
(assert-eq "and the nodes that were started are no longer seen" gn_were (length (lisp-nodes)))

;a ring of 5, this node and four more. The node two round from here one way
;is three round the other, and only the short way is held. The node next to
;this one on the short way goes. The one behind it can not be reached, and
;is not listed, till it is heard by the long way, which the kick brings on.
;The one that went is not listed again, sys/link/class.vp, +node_hops_none
(defq gn_before (lisp-nodes) gn_pids (node-net :ring 5 0 :nil :nil "gn_5"))
(gn-seen (+ (length gn_before) 4))
(defq gn_ring (filter (# (not (find %0 gn_before))) (lisp-nodes)))
(assert-eq "a ring of 5 is four more" 4 (length gn_ring))
(task-sleep 800000)
(defq gn_victim (some (lambda (node)
	(open-task (str `(mail-send (hex-decode ,(hex-encode gn_mbox)) (str (pii-pid)))) node +kn_call_pin 0 (mail-mbox))
	(if (eql (mail-read-timeout gn_mbox (task-timeout 5)) (str (first gn_pids))) node)) gn_ring)
	gn_others (filter (# (not (eql %0 gn_victim))) gn_ring))
(assert-true "the first of them is next to this node" gn_victim)
(open-task "(pii-exit)" gn_victim +kn_call_pin 0 (mail-mbox))
(gn-gone (list (first gn_pids)))
(defq gn_t0 (pii-time))
(while (and (or (find gn_victim (lisp-nodes)) (notevery (# (find %0 (lisp-nodes))) gn_others))
		(< (- (pii-time) gn_t0) (task-timeout 4)))
	(task-sleep 20000))
(assert-eq "the one that went is not listed" :nil (find gn_victim (lisp-nodes)))
(assert-true "the three that are there are, the one behind it by the long way round" (every (# (find %0 (lisp-nodes))) gn_others))
(each (# (open-task (str `(mail-send (hex-decode ,(hex-encode gn_mbox)) "there")) %0 +kn_call_pin 0 (mail-mbox))) gn_others)
(assert-list-eq "and a task sent to each of them runs" '("there" "there" "there")
	(map (lambda (&) (ifn (mail-read-timeout gn_mbox (task-timeout 5)) "none")) gn_others))
;each waits a moment before it goes. One of them is the way to the other
;two now, and if it went at once what it had yet to pass on went with it
(each (# (open-task "(progn (task-sleep 300000) (pii-exit))" %0 +kn_call_pin 0 (mail-mbox))) gn_others)
(node-stop "gn_5")
(assert-true "the ring of 5 is stopped" (gn-gone gn_pids))
(defq gn_t0 (pii-time))
(while (and (> (length (lisp-nodes)) gn_were) (< (- (pii-time) gn_t0) (task-timeout 15)))
	(task-sleep 100000))
(assert-eq "and none of it is seen" gn_were (length (lisp-nodes)))
