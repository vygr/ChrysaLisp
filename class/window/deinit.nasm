%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'

	fn_function class/window/deinit
		;inputs
		;r0 = window object
		;trashes
		;r1-r3, r5-r15

		;save object
		vp_push r0

		;deinit parent
		vp_pop r0
		super_call window, deinit
		vp_ret

	fn_function_end
