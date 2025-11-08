;editor RPC calls

(enums +edit_rpc_type 0
	(enum jump))

(structure +edit_rpc 0
	(netid reply_id)
	(uint type))

(structure +edit_rpc_jump +edit_rpc_size
	(offset names))

(defun edit-jump-rpc (brk_id file_name line_num)
	; (edit-jump-rpc string)
	(when (nempty? (defq service (mail-enquire "Edit,")))
		(defq service (to-net-id (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service
			(setf-> (cat (str-alloc +edit_rpc_jump_size) brk_id " " file_name " " line_num)
				(+edit_rpc_reply_id mbox)
				(+edit_rpc_type +edit_rpc_type_jump)))
		(mail-read mbox)))
