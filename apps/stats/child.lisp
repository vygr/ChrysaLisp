;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

(defun-bind main ()
	;read args from parent
	(while (/= 0 (defq mbox (get-long (mail-read (task-mailbox)) 0)))
		(mail-send (str (mem-stats 0)) mbox)))
