%include 'inc/func.inc'
%include 'class/class_text.inc'

	fn_function class/text/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new text object
		static_call text, new, {}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, text
			static_call text, init, {r0, @_function_}, {r1}
			if r1, ==, 0
				;error with init
				method_call text, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
