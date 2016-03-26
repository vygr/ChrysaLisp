%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/deinit
		;inputs
		;r0 = window object
		;trashes
		;all but r0, r4

		;deinit parent
		super_call window, deinit
		vp_ret

	fn_function_end
