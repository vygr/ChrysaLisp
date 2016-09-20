%include 'inc/func.inc'

	def_function sys/mail_deinit

		;deinit mail message heap
		s_bind sys_mail, statics, r0
		s_jmp sys_heap, deinit, {[r0 + ml_statics_heap]}

	def_function_end
