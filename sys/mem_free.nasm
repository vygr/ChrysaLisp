%include 'inc/func.inc'
%include 'inc/heap.inc'

	def_function sys/mem_free
		;inputs
		;r0 = address
		;trashes
		;r0-r2

		if r0, !=, 0
			vp_sub ptr_size, r0
			vp_cpy [r0], r1
			hp_freecell r1, r0, r2
		endif
		vp_ret

	def_function_end
