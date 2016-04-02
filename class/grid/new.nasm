%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_grid.inc'

	fn_function class/grid/new
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;allocate new grid object
		vp_cpy grid_size, r0
		static_call sys_mem, alloc
		if r0, !=, 0
			;clear object memory
			vp_cpy r0, r3
			vp_cpy grid_size, r1
			static_call sys_mem, clear
			vp_cpy r3, r0
		endif
		vp_ret

	fn_function_end
