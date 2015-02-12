%include "func.inc"

	fn_function "sys/mail_deinit_mailer"

		;deinit mail message heap
		fn_call sys/mail_get_statics
		fn_jmp sys/heap_deinit

	fn_function_end
