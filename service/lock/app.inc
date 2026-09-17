;lock RPC calls

(enums +lock_type 0
	(enum claim release))

(structure +lock_rpc 0
	(netid reply_id)
	(uint type))

(structure +lock_claim +lock_rpc_size)
(structure +lock_release +lock_rpc_size)

(defun lock-claim-rpc (key &optional timeout)
	; (lock-claim-rpc key [timeout_us])
	(setd timeout (task-timeout 10))
	(when (nempty? (defq service (mail-enquire "@Lock,")))
		(defq service (hex-decode (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +lock_claim_size) key)
			(+lock_rpc_reply_id mbox)
			(+lock_rpc_type +lock_type_claim)))
		(mail-read-timeout mbox timeout)))

(defun lock-release-rpc (key &optional timeout)
	; (lock-release-rpc key [timeout_us])
	(setd timeout (task-timeout 5))
	(when (nempty? (defq service (mail-enquire "@Lock,")))
		(defq service (hex-decode (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +lock_release_size) key)
			(+lock_rpc_reply_id mbox)
			(+lock_rpc_type +lock_type_release)))
		(mail-read-timeout mbox timeout)))
