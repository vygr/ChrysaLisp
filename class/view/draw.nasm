%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

	fn_function class/view/draw
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
			def_long	local_ctx
		def_structure_end

		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_ctx]

		static_call view, get_color
		vp_cpy [r4 + local_ctx], r0
		static_call gui_ctx, set_color

		vp_cpy [r4 + local_ctx], r0
		vp_cpy [r4 + local_inst], r1
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r1 + view_w], r10
		vp_cpy [r1 + view_h], r11
		static_call gui_ctx, filled_box

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
