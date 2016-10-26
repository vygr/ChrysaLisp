%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'

	def_func class/boxed_ptr/get_flags
		;inputs
		;r0 = object
		;outputs
		;r0 = object
		;r1 = value

		vp_cpy [r0 + boxed_ptr_flags], r1
		vp_ret

	def_func_end
