%include 'inc/func.inc'
%include 'inc/syscall.inc'

	fn_function sys/write_string
		;inputs
		;r0 = string
		;r1 = fd
		;trashes
		;r2-r3

		vp_cpy r1, r3
		fn_call sys/string_length
		sys_write_string r3, r0, r1
		vp_ret

	fn_function_end
