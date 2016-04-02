%include 'inc/func.inc'
%include 'class/class_grid.inc'

	fn_function class/grid/set_grid_flags
		;inputs
		;r0 = view object
		;r1 = flags

		vp_cpy r1, [r0 + grid_flags]
		vp_ret

	fn_function_end
