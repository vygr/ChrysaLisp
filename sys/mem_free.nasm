%include 'inc/func.inc'
%include 'inc/heap.inc'

	fn_function "sys/mem_free"
		;inputs
		;r0 = address
		;trashes
		;r0-r2

		if r0, !=, 0
			vp_sub 8, r0
			vp_cpy [r0], r1
			hp_freecell r1, r0, r2
		endif
		vp_ret

	fn_function_end
