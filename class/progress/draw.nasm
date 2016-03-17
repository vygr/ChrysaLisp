%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_progress.inc'

	fn_function class/progress/draw
		;inputs
		;r0 = window object
		;r1 = ctx object
		;trashes
		;all but r4

		def_structure draw
			def_long	draw_view
			def_long	draw_ctx
		def_structure_end

		vp_sub draw_size, r4
		vp_cpy r0, [r4 + draw_view]
		vp_cpy r1, [r4 + draw_ctx]

		;draw outline
		static_call view, get_color
		vp_cpy [r4 + draw_ctx], r0
		static_call ctx, set_color
		vp_cpy [r4 + draw_view], r0
		static_call progress, get_bounds
		vp_xor r8, r8
		vp_xor r9, r9
		vp_cpy [r4 + draw_ctx], r0
		static_call ctx, box

		;darker colour
		vp_cpy [r4 + draw_view], r0
		static_call view, get_color
		vp_shr 1, r8
		vp_shr 1, r9
		vp_shr 1, r10
		vp_cpy [r4 + draw_ctx], r0
		static_call ctx, set_color

		;draw middle
		vp_cpy [r4 + draw_view], r0
		static_call progress, get_bounds
		vp_cpy progress_border_size, r8
		vp_cpy progress_border_size, r9
		vp_sub progress_border_size * 2, r10
		vp_sub progress_border_size * 2, r11
		vp_cpy [r4 + draw_ctx], r0
		vp_add draw_size, r4
		static_jmp ctx, filled_box

	fn_function_end
