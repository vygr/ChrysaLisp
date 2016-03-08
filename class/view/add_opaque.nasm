%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/add_opaque
		;inputs
		;r0 = view object
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all but r4

		;paste opaque region
		vp_add r8, r10
		vp_add r9, r11
		vp_lea [r0 + view_opaque_region], r1
		static_bind gui, statics, r0
		vp_lea [r0 + gui_statics_rect_heap], r0
		static_jmp region, paste

	fn_function_end
