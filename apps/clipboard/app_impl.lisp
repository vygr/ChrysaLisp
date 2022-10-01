(import "./app.inc")

(defun main ()
	(defq clip_service (mail-declare (task-mailbox) "CLIPBOARD_SERVICE" "Clipboard Service 0.2")
		clipboard "")
	(while :t
		(let ((msg (mail-read (task-mailbox))))
			(cond
				((= (defq type (elem-get 0 msg)) +clip_type_put)
					;put string on clipboard
					(setq clipboard (elem-get 1 msg)))
				((= type +clip_type_get)
					;get string from clipboard
					(mail-send (elem-get 1 msg) clipboard)))))
	(mail-forget clip_service))
