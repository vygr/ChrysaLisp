%include 'inc/func.inc'
%include 'class/class_pair.inc'

	def_function class/pair/get_second
		;inputs
		;r0 = pair object
		;outputs
		;r0 = pair object
		;r1 = object pointer

		vp_cpy [r0 + pair_second], r1
		vp_ret

	def_function_end
