%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	fn_function class/string/create
		;inputs
		;r0 = c string pointer
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5-r7

		;get size of string
		static_call sys_string, length, {r0}, {r1}
		vp_cpy r0, r6
		vp_cpy r1, r7

		;create new string object
		static_call string, new, {&[r1 + string_size + 1]}, {r0}
		if r0, !=, 0
			;init the object
			slot_function class, string
			static_call string, init, {r0, @_function_, r6, r7}, {r1}
			if r1, ==, 0
				;error with init
				method_call string, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
