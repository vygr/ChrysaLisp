%include 'inc/func.inc'
%include 'class/class_window.inc'

	fn_function class/window/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new window object
		static_call window, new
		if r0, !=, 0
			;init the object
			static_bind class, window, r1
			static_call window, init
			if r1, ==, 0
				;error with init
				method_call window, delete, '', '', r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
