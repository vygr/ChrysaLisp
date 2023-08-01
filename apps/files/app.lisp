(import "sys/lisp.inc")

(defun main ()
	;create child and send args, wait reply
	(mail-send (defq picker (open-child "apps/files/child.lisp" +kn_call_open))
		(list (task-netid) "Files" "." ""))
	(mail-read (task-netid))
	(mail-send picker ""))
