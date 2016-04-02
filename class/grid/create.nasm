%include 'inc/func.inc'
%include 'class/class_grid.inc'

	fn_function class/grid/create
		;outputs
		;r0 = 0 if error, else object
		;trashes
		;r1-r3

		;create new grid object
		static_call grid, new
		if r0, !=, 0
			;init the object
			static_bind class, grid, r1
			static_call grid, init
			if r1, ==, 0
				;error with init
				method_call grid, delete, r1
				vp_xor r0, r0
			endif
		endif
		vp_ret

	fn_function_end
