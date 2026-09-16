(import "service/net/app.inc")

(defun scope-rank (sc)
	; (scope-rank scope_str) -> int
	(cond
		((eql sc "Global") 0)
		((eql sc "System") 1)
		(:t 2)))

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

(defq my_sys_id (hex-encode (system-id)) local_nodes (lisp-nodes))
(print "Local nodes booted   : " (length local_nodes)
	" (System: " (slice my_sys_id 0 8) " " (cpu) "/" (abi) "/" (os) ")")

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

(defq all_nodes (lisp-nodes) reply_mbox (mail-mbox) launch_mbox (mail-mbox) timeout_mbox (mail-mbox)
	expected (length all_nodes) received 0 bad_nodes (list) local_count 0 remote_count 0
	select (list reply_mbox timeout_mbox launch_mbox)
	systems (Fmap) services_map (Fmap))

(print "\nProbing kernel statistics & machine architecture on all " expected " nodes...")

; 4. Asynchronously launch probe on each node using +kn_call_pin (non-blocking)
(each (# (defq n %0)
	(defq task_code (str `(progn
		(bind '(task_count mem_used mem_avail max_stack) (kernel-stats))
		(defq sys_id (system-id) svcs (mail-enquire "")
			host_cpu (cpu) host_os (os) host_abi (abi))
		(mail-send (hex-decode ,(hex-encode reply_mbox))
			(str (list (hex-encode (task-nodeid)) (hex-encode sys_id)
				task_count mem_used mem_avail max_stack
				host_cpu host_abi host_os svcs))))))
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
			(bind '(node_hex sys_hex tc mu ma st host_cpu host_os host_abi parsed_svcs)
				(first (read (string-stream msg))))
			(defq is_local (eql sys_hex my_sys_id)
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
			; Track services from this node
			(when (nempty? parsed_svcs)
				(each (# (defq entry %0)
					(when (nempty? entry)
						(defq e_parts (if (list?? entry) entry (split entry ",")))
						(when (>= (length e_parts) 3)
							(defq s_name (elem-get e_parts 0)
								s_netid (elem-get e_parts 1)
								s_sys (elem-get e_parts 2)
								s_info (if (> (length e_parts) 3) (join (slice e_parts 3 -1) ",") "")
								scope (cond
									((starts-with "*" s_name) "Global")
									((starts-with "@" s_name) "System")
									(:t "Local"))
								s_key (cat s_name "," s_netid))
							(unless (. services_map :find s_key)
								(. services_map :insert s_key
									(list scope s_name
										(slice s_netid 16 (min (length s_netid) 32))
										(slice s_sys 0 (min (length s_sys) 8))
										s_info))))))
					parsed_svcs))
			(print "  [" (pad (inc received) 2) "/" expected "] "
				(if is_local "[LOCAL ]" "[REMOTE]")
				" Node=" (slice node_hex 0 12) "..."
				" Sys=" sys_short
				" (" (rpad arch 21) ") "
				" Tasks=" (pad tc 2)
				" Mem=" (pad (/ mu 1024) 4) "k / "
				(pad (/ ma 1024) 4) "k"
				" Stack=" (pad st 5)
				(if (<= tc 0) " <--- BAD TASK COUNT!" ""))
			(stream-flush (io-stream "stdout"))
			(setq received (inc received)))
		((= idx 2)
			; Launch acknowledgment (drain launch_mbox)
			(mail-read launch_mbox))
		(:t ; Timeout
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
(print "Unique services : " (length (. services_map :tolist)))

(print "\nMachines in Cluster:")
(. systems :each (lambda (sys_id info)
	(print "  System " sys_id " [" (rpad (elem-get info 1) 6) "]: "
		(rpad (elem-get info 0) 22) ": "
		(pad (elem-get info 2) 2) " node(s)")))

; 7. Cluster Services Directory
(defq svcs_list (. services_map :tolist))
(print "\n=== Cluster Services (" (length svcs_list) " declared) ===")
(print "  Scope   Service              Node ID         Sys ID   Info")
(print "  ------  -------------------  --------------  -------  --------------------")
(defq sorted_svcs (sort (map (const second) svcs_list) (lambda (a b)
	(defq oa (scope-rank (first a)) ob (scope-rank (first b)))
	(if (/= oa ob)
		(- oa ob)
		(cmp (second a) (second b))))))
(each (lambda ((scope s_name s_node s_sys s_info))
	(print "  " (rpad scope 6) "  "
		(rpad (slice s_name 0 (min (length s_name) 19)) 19) "  "
		(rpad s_node 14) "  "
		(rpad (slice s_sys 0 (min (length s_sys) 8)) 7) "  "
		s_info))
	sorted_svcs)

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
