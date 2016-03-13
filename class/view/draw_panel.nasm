%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_view.inc'

%define bevel_size 2

	fn_function class/view/draw_panel
		;inputs
		;r0 = view object
		;r1 = ctx object
		;trashes
		;all but r4

		def_structure draw_panel
			def_long	draw_panel_view
			def_long	draw_panel_ctx
		def_structure_end

		vp_sub draw_panel_size, r4
		vp_cpy r0, [r4 + draw_panel_view]
		vp_cpy r1, [r4 + draw_panel_ctx]

		vp_cpy [r0 + view_red], r8
		vp_cpy [r0 + view_green], r9
		vp_cpy [r0 + view_blue], r10
		vp_cpy [r0 + view_alpha], r11
		vp_cpy r1, r0
		static_call ctx, set_color

		;fill middle
		vp_cpy [r4 + draw_panel_view], r0
		vp_cpy bevel_size, r8
		vp_cpy bevel_size, r9
		vp_cpy [r0 + view_w], r10
		vp_cpy [r0 + view_h], r11
		vp_sub bevel_size * 2, r10
		vp_sub bevel_size * 2, r11
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, filled_box

		;brighter colour
		vp_cpy [r4 + draw_panel_view], r0
		vp_cpy [r0 + view_red], r8
		vp_cpy [r0 + view_green], r9
		vp_cpy [r0 + view_blue], r10
		vp_cpy [r0 + view_alpha], r11
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
		vp_cpy bevel_size, r11
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, filled_box

		vp_cpy [r4 + draw_panel_view], r0
		vp_xor r8, r8
		vp_cpy bevel_size, r9
		vp_cpy bevel_size, r10
		vp_cpy [r0 + view_h], r11
		vp_sub bevel_size, r11
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, filled_box

		;darker colour
		vp_cpy [r4 + draw_panel_view], r0
		vp_cpy [r0 + view_red], r8
		vp_cpy [r0 + view_green], r9
		vp_cpy [r0 + view_blue], r10
		vp_cpy [r0 + view_alpha], r11
		vp_shr 1, r8
		vp_shr 1, r9
		vp_shr 1, r10
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, set_color

		;fill bottom edge and right
		vp_cpy [r4 + draw_panel_view], r0
		vp_cpy bevel_size, r8
		vp_cpy [r0 + view_h], r9
		vp_sub bevel_size, r9
		vp_cpy [r0 + view_w], r10
		vp_sub bevel_size, r10
		vp_cpy bevel_size, r11
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, filled_box

		vp_cpy [r4 + draw_panel_view], r0
		vp_cpy [r0 + view_w], r8
		vp_sub bevel_size, r8
		vp_cpy bevel_size, r9
		vp_cpy bevel_size, r10
		vp_cpy [r0 + view_h], r11
		vp_sub bevel_size * 2, r11
		vp_cpy [r4 + draw_panel_ctx], r0
		static_call ctx, filled_box

		vp_add draw_panel_size, r4
		vp_ret

	fn_function_end
