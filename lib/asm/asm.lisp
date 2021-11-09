;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

(enums +select 0
	(enum main timeout))

(structure +job 0
	(long key)
	(netid reply)
	(offset params))

;redirect print
(defun print (&rest args)
	(push msg (apply str (push args (ascii-char 10)))))

;(import "lib/debug/profile.inc")

(defun main ()
	(defq select (alloc-select +select_size) working t +timeout 5000000)
	(while working
		(mail-timeout (elem-get +select_timeout select) +timeout 0)
		(defq msg (mail-read (elem-get (defq idx (mail-select select)) select)))
		(cond
			;timeout or quit
			((or (= idx +select_timeout) (eql msg ""))
				(setq working nil))
			;main mailbox
			((= idx +select_main)
				;clear timeout
				(mail-timeout (elem-get +select_timeout select) 0 0)
				;read job
				(defq reply_key (getf msg +job_key) reply_mbox (getf msg +job_reply))
				(bind '((files *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*) _)
					(read (string-stream (slice +job_params -1 msg)) (ascii-code " ")))
				;compile the file list and catch any errors
				(setq msg (list))
				(catch
					(within-compile-env (# (each include files)))
					(print _))
				;send reply
				(print reply_key)
				(mail-send reply_mbox (apply cat msg)))))
	;(profile-report "Asm")
	(free-select select))
