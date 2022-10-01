(import "sys/lisp.inc")

;read args from parent
(defq mbox (mail-read (task-mailbox)))

;pole pii stdin
(while :t
	(while (/= 0 (defq c (pii-read-char 0)))
		(mail-send mbox (char c)))
	(task-sleep 10000))
