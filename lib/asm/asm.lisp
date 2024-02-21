;;;;;;;;;;;;;;;;;;;;
; VP Assembler Child
;;;;;;;;;;;;;;;;;;;;

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

;debug options
(case :nil
(0 (import "lib/debug/frames.inc"))
(1 (import "lib/debug/profile.inc"))
(2 (import "lib/debug/debug.inc"))
(3 (import-from "lib/debug/profile.inc" '(profile-report))))

(defun main ()
	(defq *select* (alloc-select +_select_size) *working* :t *msg* :nil)
	(within-compile-env (lambda ()
		(while *working*
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
						(first (read (string-stream (slice +_job_params -1 *msg*)))))
					;compile the file list and catch any errors
					(setq *msg* (list))
					(catch (each include *files*) (progn (print _) :t))
					;send reply
					(print *reply_key*)
					(mail-send *reply_mbox* (apply (const cat) *msg*)))))))
	(free-select *select*)
	(profile-report "Asm"))
