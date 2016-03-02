%include 'class/class_ref.inc'

	fn_function 'class/ref/ref'
		;inputs
		;r0 = object
		;trashes
		;r1

		;inc ref count
		vp_cpy [r0 + REF_COUNT], r1
		vp_inc r1
		vp_cpy r1, [r0 + REF_COUNT]
		vp_ret

	fn_function_end
