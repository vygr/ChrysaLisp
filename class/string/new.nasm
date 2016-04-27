%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/new
		;inputs
		;r0 = object size
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3, r5

		;allocate new string object
		vp_cpy r0, r5
		static_call sys_mem, alloc, {r0}, {r0, r1}
		if r0, !=, 0
			;clear object memory
			vp_cpy r0, r3
			static_call sys_mem, clear, {r0, r5}
			vp_cpy r3, r0
		endif
		vp_ret

	fn_function_end
