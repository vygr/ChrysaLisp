;jit compile apps native functions
(jit "apps/netspeed/" "lisp.vp" '("vops"))

(import "./app.inc")

;native versions
(ffi "apps/netspeed/vops" vops)
; (vops) -> (vops_regs vops_memory vops_reals)

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
				;main mailbox, reset timeout and reply with info
				(mail-timeout (elem-get select +select_timeout) 0 0)
				(bind '(vops_regs vops_memory vops_reals) (vops))
				(mail-send msg (setf-> (str-alloc +reply_size)
					(+reply_node (slice (task-mbox) +long_size -1))
					(+reply_vops_regs vops_regs)
					(+reply_vops_memory vops_memory)
					(+reply_vops_reals vops_reals)))))))
