(import "service/net/app.inc")

(defun rpad (v c &optional f)
	(defq f (ifn f " ") v (str v) l (length v) c (- (max c l) l))
	(while (> c (length f)) (setq f (cat f f)))
	(cat v (slice f 0 c)))

(print "=== ChrysaLisp Cluster Node Diagnostic Query ===")

; 1. Wait for local nodes to finish booting (wait until count stops increasing)
(print "Waiting for local nodes to boot...")
(defq last_cnt 0 stable 0)
(while (< stable 8)
	(task-sleep 100000)
	(defq cnt (length (lisp-nodes)))
	(if (and (> cnt 0) (= cnt last_cnt))
		(setq stable (inc stable))
		(setq stable 0 last_cnt cnt)))

(defq my_sys_id (hex-encode (system-id))
	local_nodes (lisp-nodes))
(print "Local nodes booted   : " (length local_nodes)
	" (System: " (slice my_sys_id 0 8) " " (cpu) "/" (os) "/" (abi) ")")

; 2. Enable LAN auto-discovery listener
(print "Enabling LAN auto-discovery listener...")
(net-discover-rpc)

; 3. Wait for cluster peers to join (wait until cluster node count stops increasing)
(print "Waiting for cluster peers to join...")
(setq last_cnt (length local_nodes) stable 0)
(while (< stable 8)
	(task-sleep 500000)
	(defq cnt (length (lisp-nodes)))
	(if (and (> cnt 0) (= cnt last_cnt))
		(setq stable (inc stable))
		(setq stable 0 last_cnt cnt)))

(defq all_nodes (lisp-nodes)
	reply_mbox (mail-mbox)
	launch_mbox (mail-mbox)
	timeout_mbox (mail-mbox)
	select (list reply_mbox timeout_mbox launch_mbox)
	expected (length all_nodes)
	received 0
	bad_nodes (list)
	local_count 0
	remote_count 0
	systems (Fmap))

(print "\nProbing kernel statistics & machine architecture on all " expected " nodes...")

; 4. Asynchronously launch probe on each node using +kn_call_pin (non-blocking)
(each (# (defq n %0)
	(defq task_code (str `(progn
		(bind '(task_count mem_used mem_avail max_stack) (kernel-stats))
		(defq sys_id (system-id)
			host_cpu (cpu)
			host_os (os)
			host_abi (abi))
		(mail-send (hex-decode ,(hex-encode reply_mbox))
			(cat (hex-encode (task-nodeid)) "|"
				(hex-encode sys_id) "|"
				(str task_count) "|"
				(str mem_used) "|"
				(str mem_avail) "|"
				(str max_stack) "|"
				(str host_cpu) "|"
				(str host_os) "|"
				(str host_abi))))))
	(open-task task_code n +kn_call_pin 0 launch_mbox))
	all_nodes)

; 5. Collect results with rolling 5s timeout
(while (< received expected)
	(mail-timeout timeout_mbox 5000000 0)
	(defq idx (mail-select select))
	(cond
		((= idx 0)
			; Probe response received
			(defq msg (mail-read reply_mbox))
			(bind '(node_hex sys_hex task_count mem_used mem_avail max_stack host_cpu host_os host_abi)
				(split msg "|"))
			(defq is_local (eql sys_hex my_sys_id)
				tc (str-as-num task_count)
				sys_short (slice sys_hex 0 8)
				arch (cat host_cpu "/" host_os "/" host_abi))
			(if is_local
				(setq local_count (inc local_count))
				(setq remote_count (inc remote_count)))
			(when (<= tc 0)
				(push bad_nodes (list node_hex sys_hex tc)))
			; Track system info
			(unless (. systems :find sys_short)
				(. systems :insert sys_short (list arch (if is_local "LOCAL" "REMOTE") 0)))
			(defq sys_info (. systems :find sys_short))
			(elem-set sys_info 2 (inc (elem-get sys_info 2)))
			(print "  [" (pad (inc received) 2) "/" expected "] "
				(if is_local "[LOCAL ]" "[REMOTE]")
				" Node=" (slice node_hex 0 12) "..."
				" Sys=" sys_short
				" (" (rpad arch 21) ") "
				" Tasks=" (pad tc 2)
				" Mem=" (pad (/ (str-as-num mem_used) 1024) 4) "k / "
				(pad (/ (str-as-num mem_avail) 1024) 4) "k"
				" Stack=" (pad (str-as-num max_stack) 5)
				(if (<= tc 0) " <--- BAD TASK COUNT!" ""))
			(stream-flush (io-stream "stdout"))
			(setq received (inc received)))
		((= idx 2)
			; Launch acknowledgment (drain launch_mbox)
			(mail-read launch_mbox))
		(:t
			; Timeout
			(print "  TIMEOUT waiting for responses after " received " of " expected)
			(stream-flush (io-stream "stdout"))
			(setq received expected))))

(mail-timeout timeout_mbox 0 0)

; 6. Summary report
(print "\n=== Cluster Machine & Topology Summary ===")
(print "Total probed    : " expected)
(print "Responses recvd : " received)
(print "Local nodes     : " local_count)
(print "Remote nodes    : " remote_count)
(print "Bad task counts : " (length bad_nodes))

(print "\nMachines in Cluster:")
(. systems :each (lambda (sys_id info)
	(print "  System " sys_id " [" (rpad (elem-get info 1) 6) "]: "
		(rpad (elem-get info 0) 22) ": "
		(pad (elem-get info 2) 2) " node(s)")))

(each (# (defq b %0)
	(print "  WARNING: Node " (first b) " on Sys " (slice (second b) 0 8)
		" reported bad task count: " (third b)))
	bad_nodes)

(if (and (= received expected) (= (length bad_nodes) 0))
	(print "\n=== CLUSTER QUERY: SUCCESS ===")
	(print "\n=== CLUSTER QUERY: FAILED ==="))

(stream-flush (io-stream "stdout"))
(task-sleep 200000)
(pii-exit)
