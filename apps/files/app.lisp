(defun main ()
	;create child and send args, wait reply
	(mail-send (defq picker (open-child "apps/files/child.lisp" +kn_call_open))
		(list (task-mbox) "Files" "." ""))
	(mail-read (task-mbox))
	(mail-send picker ""))
