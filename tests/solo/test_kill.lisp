(report-header "Kill: a node that can not be told to go is ended by the host")

;this starts and stops nodes, so it is in tests/solo/, and runs on its own

;A node is stopped by being told to go, (pii-exit), a task it is sent. One
;that is cut off, or stuck in a task that never lets go, is never told, and
;was left running, (node-stop) could do nothing about it. The host can end
;a process by its id now, (pii-kill), where the host is new enough to, and
;(node-stop) leaves a task behind that ends what is still there.

(defun kl-gone (pids secs)
	;wait till these processes have gone, and not for ever
	(defq t0 (pii-time))
	(while (and (some (const pii-alive) pids) (< (- (pii-time) t0) (task-timeout secs)))
		(task-sleep 20000))
	(notany (const pii-alive) pids))

(defun kl-seen (want)
	(defq t0 (pii-time))
	(while (and (< (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 10)))
		(task-sleep 50000)))

(defun kl-unseen (want)
	;wait till no more than that many nodes are seen, and not for ever. How many are
	(defq t0 (pii-time))
	(while (and (> (length (lisp-nodes)) want) (< (- (pii-time) t0) (task-timeout 15)))
		(task-sleep 100000))
	(length (lisp-nodes)))

(assert-true "this host says how new it is, and is new enough to end a process"
	(and (= (length (split (pii-host) " ")) 4) (>= (str-as-num (last (split (pii-host) " "))) 2)))
(assert-list-eq "and is still the cpu, abi and os it was" (list (cpu) (abi) (os))
	(map (const sym) (slice (split (pii-host) " ") 0 3)))

;a node, ended
(defq kl_before (length (lisp-nodes)) kl_pids (node-spawn 1))
(kl-seen (inc kl_before))
(assert-true "a node is started" (and (> (first kl_pids) 0) (pii-alive (first kl_pids))))
(assert-eq "it is ended" :t (pii-kill (first kl_pids)))
(assert-true "and has gone" (kl-gone kl_pids 5))
(assert-eq "one that is not there is no trouble" :t (pii-kill (first kl_pids)))
;it is listed here till the link to it finds it has gone, a beat. The
;test that runs after this one may count the nodes
(assert-eq "and is no longer seen" kl_before (kl-unseen kl_before))
(assert-eq "this process is not one it will end" :nil (pii-kill (pii-pid)))
(assert-true "and it is still here to say so" (pii-alive (pii-pid)))

(cond
	((eql (os) 'Windows) (test-skip "a network with a node that is stuck" "the note of a network is not kept on Windows"))
	(:t
		;a ring by name, and one node of it stuck in a task that never lets go.
		;It can not be told to go
		(defq kl_before (lisp-nodes) kl_pids (node-net :ring 3 0 :nil :nil "kl_r"))
		(kl-seen (+ (length kl_before) 2))
		(defq kl_ring (filter (# (not (find %0 kl_before))) (lisp-nodes)))
		(assert-eq "a ring of 3 is two more" 2 (length kl_ring))
		(open-task "(while :t)" (first kl_ring) +kn_call_pin 0 (mail-mbox))
		(task-sleep 300000)
		(assert-eq "stopped by name, both are to go" 2 (node-stop "kl_r"))
		(assert-true "and both do, the one that could not be told is ended" (kl-gone kl_pids 8))
		(assert-eq "and neither is seen any longer" (length kl_before) (kl-unseen (length kl_before)))))
