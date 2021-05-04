(enums +event 0
	(enum close))

(defq select (list (task-mailbox) (mail-alloc-mbox))
	clip_service (mail-declare (elem -2 select) "CLIPBOARD_SERVICE" "Clipboard Service 0.1"))

(defun main ()
	(defq id t clipboard (list))
	(while id
		(defq idx (mail-select select) msg (mail-read (elem idx select)))
		(cond
			((/= idx 0)
				(cond
					((eql (defq req_type (first msg)) "GET")
						(unless (empty? clipboard)
							(defq reply (last clipboard))
							(mail-send (second msg) reply)))
					((eql req_type "PUT")
						(push clipboard (second msg)))))
			(t	;close
				(setq id nil))))
	;shutdown, if requested.
	(mail-free-mbox (pop select))
	(mail-forget clip_service))
