%include 'inc/func.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

	fn_function class/window/set_title
		;inputs
		;r0 = window object
		;r1 = title string
		;trashes
		;r0

		vp_cpy [r0 + window_title], r0
		static_jmp title, set_text

	fn_function_end
