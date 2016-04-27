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
		set_src r0, r1
		set_dst r6, r7
		map_src_to_dst

		;get size of strings
		vp_cpy [r0 + string_length], r0
		vp_add [r1 + string_length], r0
		vp_add string_size + 1, r0

		;create new string object
		static_call string, new
		if r0, !=, 0
			;init the object
			slot_function class, string
			static_call string, init1, {r0, @_function_, r6, r7}
			if r1, ==, 0
				;error with init
				method_call string, delete, {r0}, {}, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
