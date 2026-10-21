;editor RPC calls

(enums +edit_rpc_type 0
	(enum jump))

(structure +edit_rpc 0
	(netid reply_id)
	(uint type))

(structure +edit_rpc_jump +edit_rpc_size
	(offset names))

(defun edit-jump-rpc (file_name line_num)
	; (edit-jump-rpc file line) -> :nil | ""
	;the Editor, if it is running, opens the file at the line
	(when (nempty? (defq service (mail-enquire "@Edit,")))
		(defq service (hex-decode (second (split (pop service) ",")))
			mbox (mail-mbox))
		(mail-send service
			(setf-> (cat (str-alloc +edit_rpc_jump_size) file_name "|" (str line_num))
				(+edit_rpc_reply_id mbox)
				(+edit_rpc_type +edit_rpc_type_jump)))
		(mail-read-timeout mbox)))
