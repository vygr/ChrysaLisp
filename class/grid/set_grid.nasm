%include 'inc/func.inc'
%include 'class/class_grid.inc'

	def_function class/grid/set_grid
		;inputs
		;r0 = view object
		;r10 = width
		;r11 = height

		vp_cpy r10, [r0 + grid_width]
		vp_cpy r11, [r0 + grid_height]
		vp_ret

	def_function_end
