;lock RPC calls

(enums +lock_type 0
	(enum claim release))

(enums +lock_mode 0
	(enum write read))

(structure +lock_rpc 0
	(netid reply_id)
	(uint type mode)
	(long timeout))

(structure +lock_claim +lock_rpc_size)
(structure +lock_release +lock_rpc_size)

(defun lock-claim-rpc (key &optional mode timeout)
	; (lock-claim-rpc key [mode timeout_us]) -> :t | :nil
	(setd mode +lock_mode_write timeout (task-timeout 10))
	(if (and (nempty? (defq service (mail-enquire "@Lock,")))
			(defq service (hex-decode (second (split (pop service) ","))) mbox (mail-mbox))
			(mail-send service (setf-> (cat (str-alloc +lock_claim_size) key)
				(+lock_rpc_reply_id mbox)
				(+lock_rpc_type +lock_type_claim)
				(+lock_rpc_mode mode)
				(+lock_rpc_timeout timeout)))
			(mail-read-timeout mbox timeout))
		:t :nil))

(defun lock-release-rpc (key &optional timeout)
	; (lock-release-rpc key [timeout_us]) -> :t | :nil
	(if (and (nempty? (defq service (mail-enquire "@Lock,")))
			(defq service (hex-decode (second (split (pop service) ","))) mbox (mail-mbox))
			(mail-send service (setf-> (cat (str-alloc +lock_release_size) key)
				(+lock_rpc_reply_id mbox)
				(+lock_rpc_type +lock_type_release)))
			(mail-read-timeout mbox (ifn timeout (task-timeout 5))))
		:t :nil))