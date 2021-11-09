;clipboard messages

(enums +clip_type 0
	(enum put get))

(defun clipboard-put (string)
	; (clipboard-put string)
	(when (/= 0 (length (defq services (mail-enquire "CLIPBOARD_SERVICE"))))
		(setq services (filter (# (eql
			(slice +long_size -1 (task-mailbox))
			(slice +long_size -1 %0))) (map (# (to-net-id (elem-get 1 (split %0 ",")))) services)))
		(mail-send (pop services) (list +clip_type_put string))))

(defun clipboard-get ()
	; (clipboard-get) -> string
	(defq data "")
	(when (/= 0 (length (defq services (mail-enquire "CLIPBOARD_SERVICE"))))
		(setq services (filter (# (eql
			(slice +long_size -1 (task-mailbox))
			(slice +long_size -1 %0))) (map (# (to-net-id (elem-get 1 (split %0 ",")))) services)))
		(defq mbox (mail-alloc-mbox))
		(mail-send (pop services) (list +clip_type_get mbox))
		(setq data (mail-read mbox))
		(mail-free-mbox mbox))
	data)
