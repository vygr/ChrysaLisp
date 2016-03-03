%include 'inc/func.inc'
%include 'inc/syscall.inc'

	fn_function "sys/write_char"
		;inputs
		;r0 = char
		;r1 = fd

		sys_write_char r1, r0
		vp_ret

	fn_function_end
