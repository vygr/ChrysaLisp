%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'

	fn_function class/window/layout
		;inputs
		;r0 = window object
		;trashes
		;all but r4

		vp_cpy window_border_size, r8
		vp_cpy window_border_size, r9
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub window_border_size * 2, r10
		vp_sub window_border_size * 2, r11
		vp_cpy [r0 + window_flow], r0
		static_jmp flow, change

	fn_function_end
