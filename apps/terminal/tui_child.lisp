;imports
(import 'sys/lisp.inc)

;read args from parent
(bind '(mbox) (mail-mymail))

;pole pii stdin
(while t
	(while (/= 0 (defq c (pii-read-char 0)))
		(mail-send (char c) mbox))
	(task-sleep 10000))
