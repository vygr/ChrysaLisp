(defq *app_root* (slice (first (repl-info)) 0 (rfind "/" (first (repl-info)))))

(defun main ()
	;create child and send args, wait reply
	(mail-send (defq picker (open-child (cat *app_root* "child.lisp") +kn_call_open))
		(list (task-mbox) "Files" "." ""))
	(mail-read (task-mbox))
	(mail-send picker ""))
