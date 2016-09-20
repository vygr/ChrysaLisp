%include 'inc/func.inc'
%include 'class/class_window.inc'

	def_function class/window/get_panel
		;inputs
		;r0 = window object
		;outputs
		;r0 = window object
		;r1 = window panel object

		vp_cpy [r0 + window_panel], r1
		vp_ret

	def_function_end
