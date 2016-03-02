%include 'class/class_ref.inc'

	fn_function 'class/ref/init'
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok

		;init parent
		function_call obj, init
		if r1, !=, 0
			;init myself
			vp_cpy 1, qword[r0 + REF_COUNT]
		endif
		vp_ret

	fn_function_end
