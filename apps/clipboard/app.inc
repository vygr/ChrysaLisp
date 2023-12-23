;clipboard messages

(enums +clip_type 0
	(enum put get))

(defun clipboard-put (string)
	; (clipboard-put string)
	(when (/= 0 (length (defq service (mail-enquire "Clipboard,"))))
		(setq service (to-net-id (second (split-str (pop service) ","))))
		(mail-send service (list +clip_type_put string))))

(defun clipboard-get ()
	; (clipboard-get) -> string
	(defq data "")
	(when (/= 0 (length (defq service (mail-enquire "Clipboard,"))))
		(setq service (to-net-id (second (split-str (pop service) ","))))
		(defq mbox (mail-alloc-mbox))
		(mail-send service (list +clip_type_get mbox))
		(setq data (mail-read mbox))
		(mail-free-mbox mbox))
	data)
