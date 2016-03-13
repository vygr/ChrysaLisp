%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/change
		;inputs
		;r0 = view object
		;r8 = new x
		;r9 = new y
		;r10 = new w
		;r11 = new h
		;trashes
		;all but r4

		;save old info
		vp_cpy [r0 + view_x], r12
		vp_cpy [r0 + view_y], r13
		vp_cpy [r0 + view_w], r14
		vp_cpy [r0 + view_h], r15

		;set new info
		vp_cpy r8, [r0 + view_x]
		vp_cpy r9, [r0 + view_y]
		vp_cpy r10, [r0 + view_w]
		vp_cpy r11, [r0 + view_h]

		;layout if changed size
		if r10, ==, r14
			if r11, ==, r15
				vp_ret
			endif
		endif
		method_jmp view, layout

	fn_function_end
