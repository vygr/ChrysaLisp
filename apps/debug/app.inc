;debug message

(structure +debug 0
	(netid reply origin)
	(offset data))

(defun debug-send (&rest s)
	; (debug-send form ...)
	(when (/= 0 (length (defq services (mail-enquire "DEBUG_SERVICE"))))
		(defq mbox (mail-alloc-mbox) debug_mbox (to-net-id (elem 1 (split (pop services) ","))))
		(mail-send debug_mbox (setf-> (cat (str-alloc +debug_size) (apply str s))
			(+debug_reply mbox)
			(+debug_origin (task-mailbox))))
		(mail-read mbox)
		(mail-free-mbox mbox)))
