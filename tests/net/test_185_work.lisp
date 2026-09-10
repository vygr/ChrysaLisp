(print "=== ChrysaLisp Remote Work Test (Machine 185) ===")

; 1. Record local node identity BEFORE connecting anything
(defq local_node (task-nodeid)
	local_nodes (lisp-nodes)
	local_count (length local_nodes)
	expected_total (+ local_count 10))

(print "Local node ID         : " (hex-encode local_node))
(print "Local nodes (-n " local_count ")  : " local_count)
(print "Expected total nodes  : " local_count " + 10 = " expected_total)

; 2. Connect link to machine 185
(print "\nConnecting link to 192.168.1.185:3333...")
(mail-send (open-child "service/net/link" +kn_call_open) "192.168.1.185:3333")

; 3. Wait until we see local_count + 10 total nodes (up to 30 seconds)
(print "Waiting for " expected_total " total nodes (" local_count " local + 10 remote)...")
(defq retries 60 last_cnt -1)
(while (> retries 0)
	(task-sleep 500000)
	(defq cur_count (length (lisp-nodes)))
	(when (/= cur_count last_cnt)
		(print "  [" (- 61 retries) "x 500ms] total nodes: " cur_count
			" (need " expected_total ")")
		(setq last_cnt cur_count))
	(when (>= cur_count expected_total)
		(task-sleep 1000000)
		(setq retries 0))
	(setq retries (- retries 1)))

; 4. Tally remote nodes
(defq all_nodes (lisp-nodes)
	remote_nodes (filter (# (not (eql %0 local_node))) all_nodes))

(print "\nFinal state:")
(print "  Total nodes    : " (length all_nodes) " (expected " expected_total ")")
(print "  Remote nodes   : " (length remote_nodes) " (expected 10)")

(when (= (length remote_nodes) 0)
	(print "\n=== RESULT: FAILED - no remote nodes discovered from 185 ===")
	((ffi "service/gui/lisp_deinit")))

; 5. Dispatch a task to EVERY remote node.
; Each task returns: node-id hex, task_count, mem_used, mem_avail, max_stack
(print "\nDispatching work to all " (length remote_nodes) " remote node(s)...")
(defq reply_mbox (mail-mbox)
	dispatched 0
	work_results (list))

(each (# (defq remote_node %0)
	(print "  -> " (hex-encode remote_node))
	(defq task_code (str `(progn
		(bind '(task_count mem_used mem_avail max_stack) (kernel-stats))
		(mail-send (hex-decode ,(hex-encode reply_mbox))
			(cat (hex-encode (task-nodeid)) "\n"
				(str task_count) "\n"
				(str mem_used) "\n"
				(str mem_avail) "\n"
				(str max_stack))))))
	(open-remote task_code remote_node +kn_call_child)
	(setq dispatched (+ dispatched 1)))
	remote_nodes)

; 6. Collect all results with a 10 second timeout
(print "\nWaiting for " dispatched " result(s)...")
(defq timeout_mbox (mail-mbox)
	select (list reply_mbox timeout_mbox)
	received 0
	all_remote :t
	timed_out :nil)

(mail-timeout timeout_mbox 10000000 0)

(while (< received dispatched)
	(defq idx (mail-select select)
		msg (mail-read (elem-get select idx)))
	(cond
		((= idx 0)
			(push work_results msg)
			(bind '(node_hex task_count mem_used mem_avail max_stack)
				(split msg "\n"))
			(defq is_local (eql node_hex (hex-encode local_node)))
			(print "  Result [" (inc received) "/" dispatched "]:"
				" node=" node_hex
				" tasks=" task_count
				" mem=" mem_used "/" mem_avail
				" stack=" max_stack
				(if is_local " <-- WRONG: LOCAL!" " (remote ok)"))
			(when is_local (setq all_remote :nil))
			(setq received (inc received)))
		(:t
			(setq timed_out :t)
			(print "  TIMEOUT - only received " received "/" dispatched " results")
			(setq received dispatched))))

(mail-timeout timeout_mbox 0 0)

; 7. Final report
(print "\n=== Remote Work Test Summary ===")
(print "Local nodes (-n)   : " local_count)
(print "Total discovered   : " (length all_nodes) " / " expected_total " expected")
(print "Remote nodes       : " (length remote_nodes) " / 10 expected")
(print "Dispatched         : " dispatched)
(print "Received           : " (length work_results))
(print "All remote         : " all_remote)
(print "Timed out          : " timed_out)

(defq pass (and
	(>= (length all_nodes) expected_total)
	(>= (length remote_nodes) 10)
	(= (length work_results) dispatched)
	all_remote
	(not timed_out)))

(if pass
	(print "\n=== RESULT: SUCCESS - " local_count " local + 10 remote nodes all working ===")
	(print "\n=== RESULT: FAILED ==="))

((ffi "service/gui/lisp_deinit"))
