%include "func.inc"

	fn_function "sys/mail_in"
		;mail fragments arriving on chip task

		loopstart
			fn_call sys/mail_read_mymail
			fn_call sys/mem_free
		loopend
		vp_ret

	fn_function_end
