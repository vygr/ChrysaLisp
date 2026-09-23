;lock RPC calls

(defq +lock_max_history 128)

(enums +lock_type 0
	(enum claim release history))

(enums +lock_mode 0
	(enum write read))

(structure +lock_rpc 0
	(netid reply_id)
	(uint type mode)
	(long timeout))

(structure +lock_claim +lock_rpc_size)
(structure +lock_release +lock_rpc_size)
(structure +lock_history +lock_rpc_size)

(defun lock-service ()
	; (lock-service) -> netid | :nil
	(when (nempty? (defq s (mail-enquire "@Lock,")))
		(hex-decode (second (split (first s) ",")))))

(defun ensure-lock-service ()
	; (ensure-lock-service) -> netid | :nil
	(defq svc (lock-service) retries 50)
	(while (and (not svc) (> (-- retries) 0))
		(task-sleep 10000)
		(setq svc (lock-service)))
	svc)

(defun lock-claim-rpc (key &optional mode timeout)
	; (lock-claim-rpc key [mode timeout_us]) -> :t | :nil
	(defq svc (ensure-lock-service) mbox (mail-mbox))
	(if (and svc
			(mail-send svc (setf-> (cat (str-alloc +lock_claim_size) key)
				(+lock_rpc_reply_id mbox)
				(+lock_rpc_type +lock_type_claim)
				(+lock_rpc_mode (ifn mode +lock_mode_write))
				(+lock_rpc_timeout (ifn timeout (task-timeout 10)))))
			(mail-read-timeout mbox (ifn timeout (task-timeout 10))))
		:t :nil))

(defun lock-release-rpc (key &optional timeout)
	; (lock-release-rpc key [timeout_us]) -> :t | :nil
	(defq svc (lock-service) mbox (mail-mbox))
	(if (and svc
			(mail-send svc (setf-> (cat (str-alloc +lock_release_size) key)
				(+lock_rpc_reply_id mbox)
				(+lock_rpc_type +lock_type_release)))
			(mail-read-timeout mbox (ifn timeout (task-timeout 5))))
		:t :nil))

(defun lock-history-rpc (&optional timeout)
	; (lock-history-rpc [timeout_us]) -> list | :nil
	(defq svc (ensure-lock-service) mbox (mail-mbox))
	(if (and svc
			(mail-send svc (setf-> (str-alloc +lock_history_size)
				(+lock_rpc_reply_id mbox)
				(+lock_rpc_type +lock_type_history)))
			(defq reply (mail-read-timeout mbox (ifn timeout (task-timeout 5)))))
		(if (empty? reply) (list) (split reply "\n"))
		:nil))

(defmacro with-lock (spec &rest body)
	; (with-lock key body ...)
	; (with-lock (key [mode timeout]) body ...)
	(defq k (gensym) r (gensym))
	(bind '(key &optional mode timeout) (if (list?? spec) spec (list spec)))
	(static-qqp (when (defq ,k ,key)
		(when (lock-claim-rpc ,k ,mode ,timeout)
			(defq ,r (progn ~body))
			(lock-release-rpc ,k)
			,r))))

(defmacro with-read-lock (spec &rest body)
	; (with-read-lock key body ...)
	; (with-read-lock (key [timeout]) body ...)
	(bind '(key &optional timeout) (if (list?? spec) spec (list spec)))
	(static-qqp (with-lock (,key +lock_mode_read ,timeout) ~body)))

(defmacro with-write-lock (spec &rest body)
	; (with-write-lock key body ...)
	; (with-write-lock (key [timeout]) body ...)
	(bind '(key &optional timeout) (if (list?? spec) spec (list spec)))
	(static-qqp (with-lock (,key +lock_mode_write ,timeout) ~body)))
