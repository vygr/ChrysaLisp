;import sys settings
(bind '(byte_size short_size int_size long_size ptr_size
	kn_call_open kn_call_child lk_data_size id_size)
	(within-compile-env (lambda ()
		(import 'sys/code.inc)
		(import 'sys/mail/mail.inc)
		(import 'sys/link/link.inc)
		(import 'sys/kernel/kernel.inc)
		(list byte_size short_size int_size long_size ptr_size
			kn_call_open kn_call_child lk_data_size id_size))))

;system lisp bindings
(ffi mail-mymail "sys/mail/lisp_mymail" 0)
(ffi mail-send "sys/mail/lisp_send" 0)
(ffi open-child "sys/task/lisp_open_child" 0)
(ffi open-remote "sys/task/lisp_open_remote" 0)
(ffi open-farm "sys/task/lisp_open_farm" 0)
(ffi open-pipe "sys/task/lisp_open_pipe" 0)
(ffi cpu-total "sys/kernel/lisp_total" 0)
(ffi task-mailbox "sys/task/lisp_mailbox" 0)
