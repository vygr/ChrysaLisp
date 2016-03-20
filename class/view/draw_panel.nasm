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
		;all but r4

		def_structure	draw_panel
			def_long	draw_panel_view
			def_long	draw_panel_ctx
			def_long	draw_panel_flags
			def_long	draw_panel_depth
		def_structure_end

		vp_sub draw_panel_size, r4
		vp_cpy r0, [r4 + draw_panel_view]
		vp_cpy r1, [r4 + draw_panel_ctx]
		vp_cpy r2, [r4 + draw_panel_flags]
		vp_cpy r3, [r4 + draw_panel_depth]

		if r2, !=, 0
			;fill middle
			static_call view, get_color
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, set_color

			vp_cpy [r4 + draw_panel_depth],r12
			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy r12, r8
			vp_cpy r12, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r10
			vp_sub r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box
		endif

		vp_cpy [r4 + draw_panel_depth],r12
		if r12, >, 0
			;brighter colour
			vp_cpy [r4 + draw_panel_view], r0
			static_call view, get_color
			vp_shr 1, r8
			vp_shr 1, r9
			vp_shr 1, r10
			vp_add 128, r8
			vp_add 128, r9
			vp_add 128, r10
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, set_color

			;fill left edge and top
			vp_cpy [r4 + draw_panel_view], r0
			vp_xor r8, r8
			vp_xor r9, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r4 + draw_panel_depth], r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_xor r8, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			;darker colour
			vp_cpy [r4 + draw_panel_view], r0
			static_call view, get_color
			vp_shr 1, r8
			vp_shr 1, r9
			vp_shr 1, r10
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, set_color

			;fill bottom edge and right
			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_cpy r12, r8
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			vp_cpy r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box
		else
			;sunken panel
			vp_mul -1, r12
			vp_cpy r12, [r4 + draw_panel_depth]

			;darker colour
			vp_cpy [r4 + draw_panel_view], r0
			static_call view, get_color
			vp_shr 1, r8
			vp_shr 1, r9
			vp_shr 1, r10
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, set_color

			;fill left edge and top
			vp_cpy [r4 + draw_panel_view], r0
			vp_xor r8, r8
			vp_xor r9, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r4 + draw_panel_depth], r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_xor r8, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			;brighter colour
			vp_cpy [r4 + draw_panel_view], r0
			static_call view, get_color
			vp_shr 1, r8
			vp_shr 1, r9
			vp_shr 1, r10
			vp_add 128, r8
			vp_add 128, r9
			vp_add 128, r10
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, set_color

			;fill bottom edge and right
			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_cpy r12, r8
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			vp_cpy r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box

			vp_cpy [r4 + draw_panel_view], r0
			vp_cpy [r4 + draw_panel_depth],r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			vp_cpy [r4 + draw_panel_ctx], r0
			static_call ctx, filled_box
		endif

		vp_add draw_panel_size, r4
		vp_ret

	fn_function_end
