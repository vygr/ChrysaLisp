%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'class/class_string.inc'

	fn_function class/string/add
		;inputs
		;r0 = string object
		;r1 = string object
		;outputs
		;r0 = 0 if error, else new string object
		;trashes
		;r1-r3, r5-r7

		;save inputs
		vp_cpy r0, r6
		vp_cpy r1, r7

		;get size of strings
		vp_cpy [r0 + string_length], r0
		vp_add [r1 + string_length], r0
		vp_inc r0

		;create new string object
		static_call string, new
		if r0, !=, 0
			;init the object
			static_bind class, string, r1
			vp_cpy r6, r2
			vp_cpy r7, r3
			static_call string, init1
			if r1, ==, 0
				;error with init
				method_call string, delete, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
