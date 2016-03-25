%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r4

		def_structure	local
			def_long	local_view
			def_long	local_ctx
			def_long	local_fill_remain
			def_long	local_fill_complete
		def_structure_end

		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_view]
		vp_cpy r1, [r4 + local_ctx]

		;draw outline
		static_call view, get_color
		vp_cpy [r4 + local_ctx], r0
		static_call ctx, set_color
		vp_cpy [r4 + local_view], r0
		static_call progress, get_bounds
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r4 + local_ctx], r0
		static_call ctx, box

		;darker colour
		vp_cpy [r4 + local_view], r0
		static_call view, get_color
		vp_shr 1, r8
		vp_shr 1, r9
		vp_shr 1, r10
		vp_cpy [r4 + local_ctx], r0
		static_call ctx, set_color

		;draw middle
		vp_cpy [r4 + local_view], r0
		static_call progress, get_bounds
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
		vp_cpy [r4 + local_ctx], r0
		static_call ctx, filled_box

		;very darker colour
		vp_cpy [r4 + local_view], r0
		static_call view, get_color
		vp_shr 2, r8
		vp_shr 2, r9
		vp_shr 2, r10
		vp_cpy [r4 + local_ctx], r0
		static_call ctx, set_color

		;draw middle
		vp_cpy [r4 + local_view], r0
		static_call progress, get_bounds
		vp_cpy progress_border_size, r8
		vp_cpy progress_border_size, r9
		vp_cpy [r4 + local_fill_remain], r10
		vp_sub progress_border_size * 2, r11
		vp_add [r4 + local_fill_complete], r8
		vp_cpy [r4 + local_ctx], r0
		vp_add local_size, r4
		static_jmp ctx, filled_box

	fn_function_end
