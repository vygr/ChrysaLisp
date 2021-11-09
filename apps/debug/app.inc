;debug message

(structure +debug 0
	(netid reply origin)
	(offset data))

(defun debug-send (&rest s)
	; (debug-send form ...)
	(when (/= 0 (length (defq services (mail-enquire "DEBUG_SERVICE"))))
		(defq mbox (mail-alloc-mbox) debug_mbox (to-net-id (elem-get 1 (split (pop services) ",")))
			s (apply str s) s (cat s (if (eql (elem-get -2 s) (ascii-char 10)) "" (ascii-char 10))))
		(mail-send debug_mbox (setf-> (cat (str-alloc +debug_size) s)
			(+debug_reply mbox)
			(+debug_origin (task-mailbox))))
		(mail-read mbox)
		(mail-free-mbox mbox)))
