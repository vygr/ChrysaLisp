%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new string object
		static_call string, new
		if r0, !=, 0
			;init the object
			static_bind class, string, r1
			static_call string, init
			if r1, ==, 0
				;error with init
				method_call string, delete, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
