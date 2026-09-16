(import "service/net/app.inc")

(print "=== ChrysaLisp Cluster Node Diagnostic Query ===")

; Wait for local nodes to finish booting
(defq last_local -1 stable_local 0)
(while (< stable_local 3)
	(task-sleep 100000)
	(defq cnt (length (lisp-nodes)))
	(if (= cnt last_local)
		(setq stable_local (inc stable_local))
		(setq stable_local 0 last_local cnt)))

(defq local_nodes (lisp-nodes))
(print "Local nodes detected : " (length local_nodes))

(print "Enabling LAN auto-discovery listener...")
(net-discover-rpc)

; Wait for peer discovery and node joining to stabilize
(print "Waiting for remote cluster peers to join...")
(defq last_cnt 0 stable 0)
(while (< stable 6)
	(task-sleep 500000)
	(defq cnt (length (lisp-nodes)))
	(if (= cnt last_cnt)
		(setq stable (inc stable))
		(setq stable 0 last_cnt cnt)))

(defq all (lisp-nodes)
	remote (filter (# (not (find %0 local_nodes))) all))

(print "\nCluster Topology:")
(print "  Total nodes : " (length all))
(print "  Local nodes : " (length local_nodes))
(print "  Remote nodes: " (length remote))

(defq reply_mbox (mail-mbox))

; Probe every single node directly using +kn_call_pin to measure exact local stats
(print "\nProbing kernel statistics on all " (length all) " nodes...")
(each (# (defq n %0)
	(defq task_code (str `(progn
		(bind '(task_count mem_used mem_avail max_stack) (kernel-stats))
		(defq sys_id (system-id))
		(mail-send (hex-decode ,(hex-encode reply_mbox))
			(cat (hex-encode (task-nodeid)) "|"
				(hex-encode sys_id) "|"
				(str task_count) "|"
				(str mem_used) "|"
				(str mem_avail) "|"
				(str max_stack))))))
	(open-remote task_code n +kn_call_pin))
	all)

(defq timeout_mbox (mail-mbox)
	select (list reply_mbox timeout_mbox)
	received 0
	expected (length all))

(mail-timeout timeout_mbox 10000000 0)

(while (< received expected)
	(defq idx (mail-select select))
	(cond
		((= idx 0)
			(defq msg (mail-read reply_mbox))
			(bind '(node_hex sys_hex task_count mem_used mem_avail max_stack)
				(split msg "|"))
			(defq is_local (find (hex-decode node_hex) local_nodes))
			(print "  [" (align (inc received) 2) "/" expected "] "
				(if is_local "[LOCAL ]" "[REMOTE]")
				" Node=" (slice node_hex 0 12) "..."
				" Sys=" (slice sys_hex 0 8)
				" Tasks=" (align task_count 2)
				" Mem=" (align (/ (str-as-num mem_used) 1024) 4) "k / "
				(align (/ (str-as-num mem_avail) 1024) 4) "k"
				" Stack=" (align max_stack 5))
			(setq received (inc received)))
		(:t
			(print "  TIMEOUT waiting for responses after " received " of " expected)
			(setq received expected))))

(mail-timeout timeout_mbox 0 0)

(print "\n=== Query Summary ===")
(print "Total probed    : " expected)
(print "Responses recvd : " received)

(if (= received expected)
	(print "\n=== CLUSTER QUERY: SUCCESS ===")
	(print "\n=== CLUSTER QUERY: INCOMPLETE ==="))

(stream-flush (io-stream "stdout"))
(pii-exit)
