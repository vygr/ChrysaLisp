(import "./app.inc")

(defun main ()
	(defq clip_service (mail-declare (task-netid) "Clipboard" "Clipboard Service 0.2")
		clipboard "")
	(while :t
		(let ((msg (mail-read (task-netid))))
			(cond
				((= (defq type (first msg)) +clip_type_put)
					;put string on clipboard
					(setq clipboard (second msg)))
				((= type +clip_type_get)
					;get string from clipboard
					(mail-send (second msg) clipboard)))))
	(mail-forget clip_service))
