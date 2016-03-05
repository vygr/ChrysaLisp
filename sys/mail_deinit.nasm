%include 'inc/func.inc'

	fn_function sys/mail_deinit, no_debug_enter

		;deinit mail message heap
		class_bind mail, statics, r0
		class_jmp heap, deinit

	fn_function_end
