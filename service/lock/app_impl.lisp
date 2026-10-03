(import "./app.inc")

(enums +select 0
	(enum main timer))

(defq +check_rate 1000000 +lock_default_lease (task-timeout 60))

(defun conflict? (key_path locks)
	(some (# (every (const eql) key_path (pfind %0 :path))) locks))

(defun node-died? (node nodes known)
	; (node-died? node nodes known) -> :t | :nil
	;a node has died if it was known and is no longer there. A node that
	;has never been seen has not died, it is just not routed to us yet, as
	;happens for the first few seconds after a network boots.
	(and (not (find node nodes)) (find node known)))

(defun purge-expired (writes reads nodes known now)
	(defq changed :nil i 0)
	; 1. purge writes if node died or lease expired
	(while (< i (length writes))
		(defq rec (elem-get writes i) node (pfind rec :node))
		(ifn (or (node-died? node nodes known)
				(> (- now (pfind rec :time)) +lock_default_lease))
			(++ i)
			(elem-set writes i (last writes))
			(pop writes)
			(setq changed :t)))
	; 2. purge reads if lease expired
	(setq i 0)
	(while (< i (length reads))
		(defq rec (elem-get reads i))
		(ifn (> (- now (pfind rec :time)) +lock_default_lease)
			(++ i)
			(elem-set reads i (last reads))
			(pop reads)
			(setq changed :t)))
	changed)

(defun log-lock-history (history action key mode)
	;the list is trimmed in place, as the caller holds it, and only once it
	;is twice the size, so the trim is not done on every lock
	(push history (cat key " (" action " " mode ")"))
	(when (>= (length history) (const (* 2 +lock_max_history)))
		(defq keep (slice history (const (neg (inc +lock_max_history))) -1))
		(clear history)
		(each (# (push history %0)) keep)))

(defun merge-locks (writes reads pending nodes known now history)
	(defq blocked (list) new_pending (list))
	(each (lambda (req)
		(defq node (pfind req :node))
		; drop request if caller node died, or it is long past its timeout.
		; A caller that gives up cancels its request, so this is only for
		; one that could not, and waits twice as long so the cancel comes first.
		(unless (or (node-died? node nodes known)
				(> (- now (pfind req :time)) (* 2 (pfind req :timeout))))
			(defq key (pfind req :key) key_path (pfind req :path) mode (pfind req :mode))
			(cond
				((= mode +lock_mode_write)
					(ifn (or (conflict? key_path writes)
							(conflict? key_path reads)
							(conflict? key_path blocked))
						(progn
							(pinsert req :time now)
							(push writes req)
							(log-lock-history history "lock" key "write")
							(mail-send (pfind req :reply) ""))
						(push blocked req)
						(push new_pending req)))
				(:t ; +lock_mode_read
					(ifn (or (conflict? key_path writes) (conflict? key_path blocked))
						(progn
							(if (defq rec (some (# (if (eql (pfind %0 :key) key) %0)) reads))
								(pinsert rec :mode (inc (pfind rec :mode)) :time now)
								(push reads (pmap :key key :path key_path :mode 1 :time now)))
							(log-lock-history history "lock" key "read")
							(mail-send (pfind req :reply) ""))
						(push blocked req)
						(push new_pending req)))))) pending)
	new_pending)

(defun main ()
	(defq select (task-mboxes +select_size) lock_service (mail-declare (task-mbox) "@Lock" "Lock Service 0.4")
		lock_writes (list) lock_reads (list) lock_pending (list) lock_history (list)
		;every node we have ever been routed to
		lock_known (list))
	(mail-timeout (elem-get select +select_timer) +check_rate 0)
	(while :t
		(let* ((idx (mail-select select)) (msg (mail-read (elem-get select idx))))
			(case idx
				(+select_main
					(bind '(reply_id type mode timeout) (getf-> msg +lock_rpc_reply_id +lock_rpc_type +lock_rpc_mode +lock_rpc_timeout))
					(defq key (slice msg +lock_rpc_size -1) key_path (split key "/"))
					(case type
						(+lock_type_claim
							(defq caller_node (task-nodeid reply_id))
							(push lock_pending (pmap :key key :path key_path :mode mode :reply reply_id
								:node caller_node :time (pii-time) :timeout timeout))
							(setq lock_pending (merge-locks lock_writes lock_reads lock_pending
								(defq nodes (lisp-nodes)) (merge lock_known nodes) (pii-time) lock_history)))
						(+lock_type_release
							; 1. check writes
							(ifn (defq idx (some (# (if (eql (pfind %0 :key) key) (!))) lock_writes))
								; 2. check reads
								(when (defq idx (some (# (if (eql (pfind %0 :key) key) (!))) lock_reads))
									(defq rec (elem-get lock_reads idx) cnt (dec (pfind rec :mode)))
									(ifn (<= cnt 0)
										(pinsert rec :mode cnt)
										(elem-set lock_reads idx (last lock_reads))
										(pop lock_reads))
									(log-lock-history lock_history "unlock" key "read"))
								(elem-set lock_writes idx (last lock_writes))
								(pop lock_writes)
								(log-lock-history lock_history "unlock" key "write"))
							(mail-send reply_id "")
							(setq lock_pending (merge-locks lock_writes lock_reads lock_pending
								(defq nodes (lisp-nodes)) (merge lock_known nodes) (pii-time) lock_history)))
						(+lock_type_cancel
							;the caller gave up waiting for a claim
							(ifn (defq idx (some (# (if (eql (pfind %0 :reply) reply_id) (!))) lock_pending))
								;not waiting, so it was granted as the caller gave up, undo that
								(cond
									((= mode +lock_mode_write)
										(when (defq idx (some (# (if (eql (pfind %0 :reply) reply_id) (!))) lock_writes))
											(elem-set lock_writes idx (last lock_writes))
											(pop lock_writes)
											(log-lock-history lock_history "unlock" key "write")))
									((defq idx (some (# (if (eql (pfind %0 :key) key) (!))) lock_reads))
										(defq rec (elem-get lock_reads idx) cnt (dec (pfind rec :mode)))
										(ifn (<= cnt 0)
											(pinsert rec :mode cnt)
											(elem-set lock_reads idx (last lock_reads))
											(pop lock_reads))
										(log-lock-history lock_history "unlock" key "read")))
								;still waiting, forget it
								(setq lock_pending (erase lock_pending idx (inc idx))))
							(setq lock_pending (merge-locks lock_writes lock_reads lock_pending
								(defq nodes (lisp-nodes)) (merge lock_known nodes) (pii-time) lock_history)))
						(+lock_type_history
							;the last +lock_max_history entries
							(mail-send reply_id (join (slice lock_history
								(max 0 (- (length lock_history) +lock_max_history)) -1) "\n")))))
				(+select_timer
					(mail-timeout (elem-get select +select_timer) +check_rate 0)
					(defq nodes (lisp-nodes) now (pii-time)
						purged (purge-expired lock_writes lock_reads nodes (merge lock_known nodes) now))
					(when (or purged (nempty? lock_pending))
						(setq lock_pending (merge-locks lock_writes lock_reads lock_pending nodes lock_known now lock_history)))))))
	(mail-forget lock_service))
