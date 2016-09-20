%include 'inc/func.inc'
%include 'class/class_pair.inc'

	def_function class/pair/deinit
		;inputs
		;r0 = pair object
		;trashes
		;all but r0, r4

		vp_push r0
		vp_cpy [r0 + pair_first], r0
		s_call ref, deref, {r0}
		vp_cpy [r4], r0
		vp_cpy [r0 + pair_second], r0
		s_call ref, deref, {r0}
		vp_pop r0
		p_jmp pair, deinit, {r0}

	def_function_end
