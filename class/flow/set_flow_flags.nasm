%include 'inc/func.inc'
%include 'class/class_flow.inc'

	def_function class/flow/set_flow_flags
		;inputs
		;r0 = view object
		;r1 = flags

		vp_cpy r1, [r0 + flow_flags]
		vp_ret

	def_function_end
