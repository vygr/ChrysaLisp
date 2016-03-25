%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_window.inc'

	fn_function class/window/set_status
		;inputs
		;r0 = window object
		;r1 = status string
		;trashes
		;r0

		vp_cpy [r0 + window_status], r0
		static_jmp label, set_text

	fn_function_end
