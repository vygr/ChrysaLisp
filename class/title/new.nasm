%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_title.inc'

	fn_function class/title/new
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;allocate new title object
		static_call sys_mem, alloc, {title_size}, {r0, _}
		if r0, !=, 0
			;clear object memory
			vp_cpy r0, r3
			static_call sys_mem, clear, {r0, title_size}
			vp_cpy r3, r0
		endif
		vp_ret

	fn_function_end
