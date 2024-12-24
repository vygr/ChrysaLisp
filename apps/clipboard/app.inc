;clipboard RPC calls

(enums +clip_type 0
	(enum put get))

(structure +clip_rpc 0
	(netid reply_id)
	(uint type))

(structure +clip_get +clip_rpc_size)
(structure +clip_put +clip_rpc_size)

(defun clip-put-rpc (string)
	; (clip-put-rpc string)
	(when (nempty? (defq service (mail-enquire "Clipboard,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (cat (str-alloc +clip_put_size) string)
			(+clip_rpc_reply_id mbox)
			(+clip_rpc_type +clip_type_put)))
		(mail-read mbox)
		(mail-free-mbox mbox)))

(defun clip-get-rpc ()
	; (clip-get-rpc) -> string
	(defq data "")
	(when (nempty? (defq service (mail-enquire "Clipboard,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-alloc-mbox))
		(mail-send service (setf-> (str-alloc +clip_get_size)
			(+clip_rpc_reply_id mbox)
			(+clip_rpc_type +clip_type_get)))
		(setq data (mail-read mbox))
		(mail-free-mbox mbox))
	data)
