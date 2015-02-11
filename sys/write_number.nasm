%include "func.inc"
%include "syscall.inc"

	fn_function "sys/write_number"
		;inputs
		;r0 = number
		;r1 = fd
		;trashes
		;r0-r3, r5

		vp_cpy 10, r3	;base
		vp_cpy r4, r5	;stack location
		repeat
			vp_xor r2, r2
			vp_div r3
			vp_push r2
		until r0, ==, 0
		repeat
			vp_pop r0
			vp_add '0', r0
			sys_write_char r1, r0
		until r5, ==, r4
		vp_ret

	fn_function_end
