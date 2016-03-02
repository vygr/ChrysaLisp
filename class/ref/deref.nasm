%include 'class/class_ref.inc'

	fn_function 'class/ref/deref'
		;inputs
		;r0 = object
		;trashes
		;r1

		;dec ref count
		vp_cpy [r0 + REF_COUNT], r1
		vp_dec r1
		vp_cpy r1, [r0 + REF_COUNT]

		;destroy if 0
		if r1, ==, 0
			method_call obj, destroy
		endif
		vp_ret

	fn_function_end
