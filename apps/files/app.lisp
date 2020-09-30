;imports
(import "sys/lisp.inc")

(defun-bind main ()
	;create child and send args, wait reply
	(mail-send (list (task-mailbox) "Files" "." "")
		(defq picker (open-child "apps/files/child.lisp" kn_call_open)))
	(mail-read (task-mailbox))
	(mail-send "" picker))
