%include 'inc/func.inc'
%include 'class/class_ref.inc'

	def_func class/ref/init
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		s_call ref, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_cpy_cl 1, [r0 + ref_count]
		endif
		vp_ret

	def_func_end
