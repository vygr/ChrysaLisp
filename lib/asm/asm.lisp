;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

(import "sys/lisp.inc")
(import "class/lisp.inc")

(defq +_timeout 5000000)

(enums +_select 0
	(enum main timeout))

(structure +_job 0
	(long key)
	(netid reply)
	(offset params))

;redirect print
(defun print (&rest args)
	(push *msg* (apply str (push args (ascii-char 10)))))

;(import "././debug/profile.inc")

(defun main ()
	(defq *select* (alloc-select +_select_size) *working* :t *msg* :nil)
	(within-compile-env
		(# (while *working*
			(mail-timeout (elem-get +_select_timeout *select*) +_timeout 0)
			(setq *msg* (mail-read (elem-get (defq *idx* (mail-select *select*)) *select*)))
			(cond
				;timeout or quit
				((or (= *idx* +_select_timeout) (eql *msg* ""))
					(setq *working* :nil))
				;main mailbox
				((= *idx* +_select_main)
					;clear timeout
					(mail-timeout (elem-get +_select_timeout *select*) 0 0)
					;read job
					(defq *reply_key* (getf *msg* +_job_key) *reply_mbox* (getf *msg* +_job_reply))
					(each (# (def *compile_env* %0 %1))
						'(*files* *abi* *cpu* *debug_mode* *debug_emit* *debug_inst*)
						(elem-get 0 (read (string-stream (slice +_job_params -1 *msg*)) (ascii-code " "))))
					;compile the file list and catch any errors
					(setq *msg* (list))
					(catch (each include *files*)
						(progn (setq *working* :nil) (print _)))
					;send reply
					(print *reply_key*)
					(mail-send *reply_mbox* (apply cat *msg*)))))))
	;(profile-report "Asm")
	(free-select *select*))
