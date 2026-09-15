(print "=== ChrysaLisp Network Loopback Remote Work Test ===")

; 1. Record local node identity BEFORE connecting
(defq local_nodes (lisp-nodes)
	local_count (length local_nodes)
	expected_remote 10
	expected_total (+ local_count expected_remote))

(print "CLIENT: Local node count = " local_count)
(print "CLIENT: Expected remote count = " expected_remote)
(print "CLIENT: Expected total nodes = " expected_total)

; 2. Connect link to server
(print "\nCLIENT: Connecting link to 127.0.0.1:4567...")
(mail-send (open-child "service/net/link" +kn_call_pin) "127.0.0.1:4567")

; 3. Wait for remote nodes to appear
(print "CLIENT: Waiting for " expected_total " total nodes (" local_count " local + " expected_remote " remote)...")
(defq retries 50 last_cnt -1)
(while (> retries 0)
	(task-sleep 100000)
	(defq cur_count (length (lisp-nodes)))
	(when (/= cur_count last_cnt)
		(print "  [" (- 51 retries) "x 100ms] total nodes: " cur_count " (need " expected_total ")")
		(setq last_cnt cur_count))
	(when (>= cur_count expected_total)
		(task-sleep 500000)
		(setq retries 0))
	(setq retries (- retries 1)))

; 4. Filter remote nodes
(defq all_nodes (lisp-nodes)
	remote_nodes (filter (# (not (find %0 local_nodes))) all_nodes))

(print "\nCLIENT: Node state:")
(print "  Total nodes  : " (length all_nodes) " (expected " expected_total ")")
(print "  Remote nodes : " (length remote_nodes) " (expected " expected_remote ")")

(when (< (length remote_nodes) expected_remote)
	(print "\n=== LOOPBACK TEST RESULT: FAILED - insufficient remote nodes discovered ===")
	(pii-exit))

; 5. Dispatch a task to EVERY remote node
(print "\nCLIENT: Dispatching work to all " (length remote_nodes) " remote node(s)...")
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
	(open-remote task_code remote_node +kn_call_run)
	(setq dispatched (+ dispatched 1)))
	remote_nodes)

; 6. Collect all results with timeout
(print "\nCLIENT: Waiting for " dispatched " result(s)...")
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
			(defq is_local (find (hex-decode node_hex) local_nodes))
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

; 7. Final summary and verification
(print "\n=== Loopback Remote Work Test Summary ===")
(print "Local nodes        : " local_count)
(print "Total discovered   : " (length all_nodes) " / " expected_total " expected")
(print "Remote nodes       : " (length remote_nodes) " / " expected_remote " expected")
(print "Dispatched         : " dispatched)
(print "Received           : " (length work_results))
(print "All remote         : " all_remote)
(print "Timed out          : " timed_out)

(defq pass (and
	(>= (length all_nodes) expected_total)
	(>= (length remote_nodes) expected_remote)
	(= (length work_results) dispatched)
	all_remote
	(not timed_out)))

(if pass
	(print "\n=== LOOPBACK TEST RESULT: SUCCESS ===")
	(print "\n=== LOOPBACK TEST RESULT: FAILED ==="))

(pii-exit)
