%include 'inc/func.inc'
%include 'class/class_boxed_long.inc'

	def_function class/boxed_long/set_value
		;inputs
		;r0 = object
		;r1 = value
		;outputs
		;r0 = object

		vp_cpy r1, [r0 + boxed_long_value]
		vp_ret

	def_function_end
