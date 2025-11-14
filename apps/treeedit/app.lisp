;;;;;;;;;;;;;;;;;;;;;;;;
; WYSIWYG Tree Editor
;;;;;;;;;;;;;;;;;;;;;;;;

(defun main ()
	; Launch tree editor child process
	(mail-send (defq child (open-child "apps/treeedit/child.lisp" +kn_call_open))
		(list (task-mbox) "Tree Editor"))
	(mail-read (task-mbox)))
