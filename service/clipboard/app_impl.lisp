(import "./app.inc")

(defun main ()
	(defq clip_service (mail-declare (task-mbox) "Clipboard" "Clipboard Service 0.2")
		clipboard "")
	(while :t
		(let* ((msg (mail-read (task-mbox))) (reply_id (getf msg +clip_rpc_reply_id)))
			(case (getf msg +clip_rpc_type)
				(+clip_type_put
					;put string on clipboard
					(setq clipboard (slice msg +clip_put_size -1))
					(mail-send reply_id ""))
				(+clip_type_get
					;get string from clipboard
					(mail-send reply_id clipboard)))))
	(mail-forget clip_service))
