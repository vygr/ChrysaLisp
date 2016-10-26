%include 'inc/func.inc'
%include 'class/class_pair.inc'

	def_func class/pair/deinit
		;inputs
		;r0 = pair object
		;trashes
		;all but r0, r4

		vp_push r0
		f_call ref, deref, {[r0 + pair_first]}
		vp_cpy [r4], r0
		f_call ref, deref, {[r0 + pair_second]}
		vp_pop r0
		s_jmp pair, deinit, {r0}

	def_func_end
