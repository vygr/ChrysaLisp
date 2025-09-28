;lock RPC calls

(enums +lock_type 0
	(enum claim release))

(structure +lock_rpc 0
	(netid reply_id)
	(uint type))

(structure +lock_claim +lock_rpc_size)
(structure +lock_release +lock_rpc_size)

(defun lock-claim-rpc (key)
	; (lock-claim-rpc key)
	(when (nempty? (defq service (mail-enquire "*Lock,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +lock_claim_size) key)
			(+lock_rpc_reply_id mbox)
			(+lock_rpc_type +lock_type_claim)))
		(mail-read mbox)))

(defun lock-release-rpc (key)
	; (lock-release-rpc key)
	(when (nempty? (defq service (mail-enquire "*Lock,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service (setf-> (cat (str-alloc +lock_release_size) key)
			(+lock_rpc_reply_id mbox)
			(+lock_rpc_type +lock_type_release)))
		(mail-read mbox)))
