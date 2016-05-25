%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure local
			ptr local_inst
			ptr local_ctx
			long local_fill_remain
			long local_fill_complete
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_ctx]
		map_src_to_dst

		;draw outline
		s_call view, get_color, {r0}, {r1}
		s_call gui_ctx, set_color, {[r4 + local_ctx], r1}
		s_call progress, get_bounds, {[r4 + local_inst]}, {_, _, r10, r11}
		s_call gui_ctx, box, {[r4 + local_ctx], 0, 0, r10, r11}

		;darker colour
		s_call view, get_color, {[r4 + local_inst]}, {r1}
		s_call gui_ctx, darker, {r1}, {r1}
		s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

		;draw middle
		s_call progress, get_bounds, {[r4 + local_inst]}, {_, _, r10, r11}
		vp_sub progress_border_size * 2, r10
		vp_sub progress_border_size * 2, r11
		vp_cpy [r0 + progress_val], r2
		vp_cpy [r0 + progress_max], r1
		vp_cpy r10, r12
		vp_cpy r10, r0
		vp_mul r2, r0
		vp_xor r2, r2
		vp_div r1, r2, r0
		vp_cpy r0, [r4 + local_fill_complete]
		vp_cpy r0, r10
		vp_sub r10, r12
		vp_cpy r12, [r4 + local_fill_remain]
		s_call gui_ctx, filled_box, {[r4 + local_ctx], progress_border_size, progress_border_size, r10, r11}

		;very darker colour
		s_call view, get_color, {[r4 + local_inst]}, {r1}
		s_call gui_ctx, darker, {r1}, {r1}
		s_call gui_ctx, darker, {r1}, {r1}
		s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

		;draw middle
		s_call progress, get_bounds, {[r4 + local_inst]}, {_, _, _, r11}
		vp_cpy progress_border_size, r8
		vp_sub progress_border_size * 2, r11
		vp_add [r4 + local_fill_complete], r8
		s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, progress_border_size, [r4 + local_fill_remain], r11}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
