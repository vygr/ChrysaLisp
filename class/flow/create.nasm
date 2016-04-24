%include 'inc/func.inc'
%include 'class/class_flow.inc'

	fn_function class/flow/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new flow object
		static_call flow, new
		if r0, !=, 0
			;init the object
			static_bind class, flow, r1
			static_call flow, init
			if r1, ==, 0
				;error with init
				method_call flow, delete, '', '', r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
