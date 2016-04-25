%include 'inc/func.inc'
%include 'class/class_title.inc'

	fn_function class/title/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new title object
		static_call title, new
		if r0, !=, 0
			;init the object
			static_bind class, title, r1
			static_call title, init
			if r1, ==, 0
				;error with init
				method_call title, delete, {}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
