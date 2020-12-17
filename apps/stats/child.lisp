;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

(defun main ()
	;read args from parent
	(until (eql "" (defq mbox (mail-read (task-mailbox))))
		(mail-send mbox (str (mem-stats 0)))))
