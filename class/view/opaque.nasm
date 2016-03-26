%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/opaque
		;inputs
		;r0 = view object
		;trashes
		;all but r0, r4

		;paste opaque region
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		static_jmp view, add_opaque

	fn_function_end
