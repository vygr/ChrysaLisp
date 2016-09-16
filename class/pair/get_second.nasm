%include 'inc/func.inc'
%include 'class/class_pair.inc'

	fn_function class/pair/get_second
		;inputs
		;r0 = pair object
		;outputs
		;r0 = pair object
		;r1 = 0, else object pointer

		vp_cpy [r0 + pair_second], r1
		vp_ret

	fn_function_end
