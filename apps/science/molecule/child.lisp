;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; apps/science/molecule/child.lisp
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defq *app_root* (path-to-file))
(import "gui/lisp.inc")
(import "./app.inc")
(import "./atoms.inc")

(enums +select 0
	(enum main timeout))

(defun main ()
	(defq select (task-mboxes +select_size) running :t +timeout 5000000)
	(while running
		(mail-timeout (elem-get select +select_timeout) +timeout 0)
		(defq msg (mail-read (elem-get select (defq idx (mail-select select)))))
		(cond
			((or (= idx +select_timeout) (eql msg ""))
				;timeout or quit
				(setq running :nil))
			((= idx +select_main)
				;main mailbox, reset timeout and reply with result
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(defq key (getf msg +job_key)
					  atom_key (getf msg +job_atom_key)
					  reply (getf msg +job_reply)
					  file (slice msg +job_file -1))
				(generate-atom-image atom_key file)
				(mail-send reply (setf-> (str-alloc +job_reply_size)
					(+job_reply_key key)))))))