%include "func.inc"
%include "heap.inc"

	fn_function "sys/mem_free"
		;inputs
		;r0 = address
		;trashes
		;r0-r2

		vp_sub 8, r0
		vp_cpy [r0], r1
		hp_free_cell r1, r0, r2
		vp_ret

	fn_function_end
