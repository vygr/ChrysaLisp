%include 'inc/func.inc'
%include 'class/class_pair.inc'

	def_func class/pair/init
		;inputs
		;r0 = pair object
		;r1 = vtable pointer
		;r2 = first object
		;r3 = second object
		;outputs
		;r1 = 0 if error, else ok

		;save inputs
		vp_cpy r2, [r0 + pair_first]
		vp_cpy r3, [r0 + pair_second]

		;init parent
		s_jmp pair, init, {r0, r1}, {r1}

	def_func_end
