%include 'inc/func.inc'
%include 'inc/syscall.inc'
%include 'inc/string.inc'

	fn_function sys/write_string
		;inputs
		;r0 = string
		;r1 = fd
		;trashes
		;r1-r3

		vp_cpy r1, r3
		s_call sys_string, length, {r0}, {r1}
		sys_write_string r3, r0, r1
		vp_ret

	fn_function_end
