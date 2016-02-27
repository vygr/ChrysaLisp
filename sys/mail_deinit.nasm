%include "func.inc"

	fn_function "sys/mail_deinit"

		;deinit mail message heap
		fn_bind sys/mail_statics, r0
		fn_jmp sys/heap_deinit

	fn_function_end
