%include 'inc/func.inc'
%include 'class/class_flow.inc'

	def_function class/flow/init
		;inputs
		;r0 = flow object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		p_call flow, init, {r0, r1}, {r1}
		if r1, !=, 0
			;init myself
			vp_cpy_cl 0, [r0 + flow_flags]
		endif
		vp_ret

	def_function_end
