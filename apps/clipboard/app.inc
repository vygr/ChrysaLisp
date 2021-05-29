;clipboard messages

(enums +clip_type 0
	(enum put get))

(structure +clipboard_event 0
	(byte type))

(structure +clipboard_put +clipboard_event_size
	(offset data))

(structure +clipboard_get +clipboard_event_size
	(netid reply))

(defun clipboard-put (string)
	; (clipboard-put string)
	(when (/= 0 (length (defq services (mail-enquire "CLIPBOARD_SERVICE"))))
		(setq services (filter (# (eql
			(slice +long_size -1 (task-mailbox))
			(slice +long_size -1 %0))) (map (# (to-net-id (elem 1 (split %0 ",")))) services)))
		(mail-send (pop services) (setf-> (cat (str-alloc +clipboard_put_size) string)
			(+clipboard_event_type +clip_type_put)))))

(defun clipboard-get ()
	; (clipboard-get) -> string
	(defq data "")
	(when (/= 0 (length (defq services (mail-enquire "CLIPBOARD_SERVICE"))))
		(setq services (filter (# (eql
			(slice +long_size -1 (task-mailbox))
			(slice +long_size -1 %0))) (map (# (to-net-id (elem 1 (split %0 ",")))) services)))
		(defq mbox (mail-alloc-mbox))
		(mail-send (pop services) (setf-> (str-alloc +clipboard_get_size)
			(+clipboard_event_type +clip_type_get)
			(+clipboard_get_reply mbox)))
		(setq data (mail-read mbox))
		(mail-free-mbox mbox))
	data)
