(defq *app_root* (path-to-file))
(import "./app.inc")
(import "./data.inc")

;The fetching of the data set, a task of its own, so the window goes on
;being drawn while it comes. Sent the mailbox to tell, it fetches each file
;not yet kept, saying +name as it starts on one, and at the end . for all
;kept, or ! and why one could not be

(defun main ()
	;who started it may be gone before it says where to tell
	(when (defq reply (mail-read-timeout (task-mbox) (task-timeout 10)))
		(defq why :nil)
		(each (lambda (file)
				(unless why
					(mail-send reply (cat "+" (first file)))
					(setq why (mnist-fetch file))))
			(mnist-missing))
		(mail-send reply (if why (cat "!" why) "."))))
