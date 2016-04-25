%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/new
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;allocate new progress object
		static_call sys_mem, alloc, {progress_size}
		if r0, !=, 0
			;clear object memory
			vp_cpy r0, r3
			static_call sys_mem, clear, {r0, progress_size}
			vp_cpy r3, r0
		endif
		vp_ret

	fn_function_end
