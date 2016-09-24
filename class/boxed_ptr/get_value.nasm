%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'

	def_function class/boxed_ptr/get_value
		;inputs
		;r0 = object
		;outputs
		;r0 = object
		;r1 = value

		vp_cpy [r0 + boxed_ptr_value], r1
		vp_ret

	def_function_end
