;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

;imports
(import 'sys/lisp.inc)
(import 'class/lisp.inc)

;read args from parent
(bind '((files mbox *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*) _)
	(read (string-stream (mail-read (task-mailbox))) (const (ascii-code " "))))

;set up reply stream
(defq msg_out (create-msg-out mbox))

;redirect print to my msg_out
(defun-bind print (&rest args)
	(write msg_out (apply str (push args (const (ascii-char 10))))))

;catch any errors
(catch
	;compile the file list
	(within-compile-env (lambda ()
		(each! 0 -1 include (list files))))
	(print _))
