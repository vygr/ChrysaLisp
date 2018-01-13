;import sys settings
(bind '(
	byte_size short_size int_size long_size ptr_size kn_call_open kn_call_child
	lk_data_size slot_mail_mymail slot_mail_send slot_open_child slot_open_remote
	slot_open_farm slot_open_pipe slot_cpu_total)
	(within-compile-env (lambda ()
		(import 'sys/mail/mail.inc)
		(import 'sys/kernel/kernel.inc)
		(import 'sys/link/link.inc)
		(import 'class/component/component.inc)
		(list
			byte_size short_size int_size long_size ptr_size
			kn_call_open kn_call_child lk_data_size
			(method-slot 'component 'mail_mymail) (method-slot 'component 'mail_send)
			(method-slot 'component 'open_child) (method-slot 'component 'open_remote)
			(method-slot 'component 'open_farm) (method-slot 'component 'open_pipe)
			(method-slot 'component 'cpu_total)
			))))

;some helpful macros
(defmacro slot (_ &rest b) `(call ,(sym-cat 'slot_ _) ~b))
