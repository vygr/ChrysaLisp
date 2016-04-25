%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_inst
			def_long	local_ctx
			def_long	local_fill_remain
			def_long	local_fill_complete
		def_structure_end

		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_ctx]

		;draw outline
		static_call view, get_color
		static_call gui_ctx, set_color, {[r4 + local_ctx], r1}
		static_call progress, get_bounds, {[r4 + local_inst]}
		static_call gui_ctx, box, {[r4 + local_ctx], 0, 0, r10, r11}

		;darker colour
		static_call view, get_color, {[r4 + local_inst]}
		vp_cpy r1, r2
		vp_cpy 0xff000000, r3
		vp_and r3, r1
		vp_and 0x00fefefe, r2
		vp_shr 1, r2
		vp_add r2, r1
		static_call gui_ctx, set_color, {[r4 + local_ctx], r1}

		;draw middle
		static_call progress, get_bounds, {[r4 + local_inst]}
		vp_cpy progress_border_size, r8
		vp_cpy progress_border_size, r9
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
		static_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

		;very darker colour
		static_call view, get_color, {[r4 + local_inst]}
		vp_cpy r1, r2
		vp_cpy 0xff000000, r3
		vp_and r3, r1
		vp_and 0x00fcfcfc, r2
		vp_shr 2, r2
		vp_add r2, r1
		static_call gui_ctx, set_color, {[r4 + local_ctx], r1}

		;draw middle
		static_call progress, get_bounds, {[r4 + local_inst]}
		vp_cpy progress_border_size, r8
		vp_cpy progress_border_size, r9
		vp_cpy [r4 + local_fill_remain], r10
		vp_sub progress_border_size * 2, r11
		vp_add [r4 + local_fill_complete], r8
		static_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
