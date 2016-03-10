%include 'inc/func.inc'
%include 'class/class_view.inc'

	fn_function class/view/move
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

		;add dirty rect in old position
		vp_sub r8, r12
		vp_sub r9, r13
		vp_cpy r12, r8
		vp_cpy r13, r9
		vp_cpy r14, r10
		vp_cpy r15, r11
		vp_push r0
		static_call view, add_dirty

		;dirty new position and children
		vp_pop r0
		static_jmp view, dirty_all

	fn_function_end
