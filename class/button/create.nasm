%include 'inc/func.inc'
%include 'class/class_button.inc'

	fn_function class/button/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new button object
		static_call button, new
		if r0, !=, 0
			;init the object
			static_bind class, button, r1
			static_call button, init
			if r1, ==, 0
				;error with init
				method_call button, delete, {}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
