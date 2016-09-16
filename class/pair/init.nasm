%include 'inc/func.inc'
%include 'class/class_pair.inc'

	fn_function class/pair/init
		;inputs
		;r0 = pair object
		;r1 = vtable pointer
		;r2 = first object
		;r3 = second object
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		;save inputs
		vp_cpy r2, [r0 + pair_first]
		vp_cpy r3, [r0 + pair_second]

		;init parent
		p_call pair, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_push r0
			s_call ref, ref, {[r0 + pair_first]}
			vp_cpy [r4], r0
			s_call ref, ref, {[r0 + pair_second]}
			vp_pop r0
		endif
		vp_ret

	fn_function_end
