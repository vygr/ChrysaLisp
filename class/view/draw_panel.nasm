%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/draw_panel
		;inputs
		;r0 = view object
		;r1 = ctx object
		;r2 = flags
		;r3 = depth
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]

		s_call view, get_bounds, {r0}, {r8, r9, r10, r11}
		vp_cpy r0, r5
		s_call gui_ctx, panel, {r1, [r5 + view_color], r2, r3, 0, 0, r10, r11}, {}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
