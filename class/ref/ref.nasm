%include 'class/class_ref.inc'

	fn_function class/ref/ref
		;inputs
		;r0 = object
		;trashes
		;r1

		;inc ref count
		vp_cpy [r0 + ref_count], r1
		vp_inc r1
		vp_cpy r1, [r0 + ref_count]
		vp_ret

	fn_function_end
