%include "func.inc"
%include "syscall.inc"

	fn_function "sys/read_line"
		;inputs
		;r0 = buffer address
		;r1 = buffer size
		;r2 = fd
		;outputs
		;r0 = chars read
		;trashes
		;r0-r3, r5

		vp_cpy r0, r3
		vp_cpy r0, r5
		vp_add r0, r1
		repeat
			breakif r5, e, r1
			sys_read_char r2
			vp_cpy r0l, byte[r5]
			vp_inc r5
		until r0, ==, 10
		vp_cpy r5, r0
		vp_sub r3, r0
		vp_ret

	fn_function_end
