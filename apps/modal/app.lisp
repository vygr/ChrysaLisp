;imports
(import "sys/lisp.inc")

(defun-bind main ()
	(defq st '())
	;create child and send args, wait reply
	(mail-send (list (task-mailbox) "Title" "Message should describe the issue." "Button1, Button2, ButtonN")
		(defq modal (open-child "apps/modal/child.lisp" kn_call_open)))
	(defq response (mail-read (task-mailbox)))
	(mail-send "" modal))