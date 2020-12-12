;imports
(import "sys/lisp.inc")

(defun main ()
	;create child and send args, wait reply
	(mail-send (list (task-mailbox) "Files" "." "")
		(defq picker (get-long (open-child "apps/files/child.lisp" kn_call_open) 0)))
	(mail-read (task-mailbox))
	(mail-send "" picker))
