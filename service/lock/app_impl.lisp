(import "./app.inc")

(enums +select 0
	(enum main timer))

(defq +check_rate 1000000 +lock_default_lease (task-timeout 60))

(defun conflict? (key_path locks)
	(some (# (every (const eql) key_path (pfind %0 :path))) locks))

(defun purge-expired (writes reads nodes now)
	(defq changed :nil i 0)
	; 1. purge writes if node died or lease expired
	(while (< i (length writes))
		(defq rec (elem-get writes i) node (pfind rec :node))
		(ifn (or (not (find node nodes))
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
	(push history (cat key " (" action " " mode ")"))
	(when (> (length history) +lock_max_history)
		(erase history 0 (- (length history) +lock_max_history))))

(defun merge-locks (writes reads pending nodes now history)
	(defq blocked (list) new_pending (list))
	(each (lambda (req)
		(defq node (pfind req :node))
		; drop request if caller node died or caller already timed out
		(unless (or (not (find node nodes))
				(> (- now (pfind req :time)) (pfind req :timeout)))
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
		lock_writes (list) lock_reads (list) lock_pending (list) lock_history (list))
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
							; merge caller node to bypass eventual consistency lag
							(setq lock_pending (merge-locks lock_writes lock_reads lock_pending (merge (lisp-nodes) (list caller_node)) (pii-time) lock_history)))
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
							(setq lock_pending (merge-locks lock_writes lock_reads lock_pending (lisp-nodes) (pii-time) lock_history)))
						(+lock_type_history
							(mail-send reply_id (join lock_history "\n")))))
				(+select_timer
					(mail-timeout (elem-get select +select_timer) +check_rate 0)
					(defq nodes (lisp-nodes) now (pii-time) purged (purge-expired lock_writes lock_reads nodes now))
					(when (or purged (nempty? lock_pending))
						(setq lock_pending (merge-locks lock_writes lock_reads lock_pending nodes now lock_history)))))))
	(mail-forget lock_service))
