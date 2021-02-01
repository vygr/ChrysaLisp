;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

;imports
(import "sys/lisp.inc")
(import "class/lisp.inc")

(structure '+select 0
	(byte 'main+ 'timeout+))

;redirect print
(defun print (&rest args)
	(push msg (apply str (push args (ascii-char 10)))))

(defun main ()
	(defq select (list (task-mailbox) (mail-alloc-mbox)) working t +timeout+ 5000000)
	(while working
		(mail-timeout (elem +select_timeout+ select) +timeout+)
		(defq msg (mail-read (elem (defq idx (mail-select select)) select)))
		(cond
			;timeout or quit
			((or (= idx +select_timeout+) (eql msg ""))
				(setq working nil))
			;main mailbox
			((= idx +select_main+)
				;clear timeout
				(mail-timeout (elem +select_timeout+ select) 0)
				;read job
				(bind '((reply files *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*) _)
					(read (string-stream msg) (ascii-code " ")))
				;compile the file list and catch any errors
				(setq msg (list))
				(catch
					(within-compile-env (# (each include files)))
					(print _))
				;send reply
				(print (to-service-id (task-mailbox)))
				(mail-send (to-net-id reply) (apply cat msg)))))
	(mail-free-mbox (pop select)))
