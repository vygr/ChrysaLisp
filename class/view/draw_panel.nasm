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

		def_local
			def_local_long	inst
			def_local_long	ctx
			def_local_long	flags
			def_local_long	depth
		def_local_end

		vp_sub local_size, r4
		vp_cpy r0, .inst
		vp_cpy r1, .ctx
		vp_cpy r2, .flags
		vp_cpy r3, .depth

		if r2, !=, 0
			;fill middle
			s_call view, get_color, {r0}, {r1}
			s_call gui_ctx, set_color, {.ctx, r1}

			vp_cpy .depth,r12
			vp_cpy .inst, r0
			vp_cpy r12, r8
			vp_cpy r12, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r10
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {.ctx, r8, r9, r10, r11}
		endif

		vp_cpy .depth,r12
		if r12, >, 0
			;brighter colour
			s_call view, get_color, {.inst}, {r1}
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add 0x00808080, r2
			vp_add r2, r1
			s_call gui_ctx, set_color, {.ctx, r1}

			;fill left edge and top
			vp_cpy .inst, r0
			s_call gui_ctx, filled_box, {.ctx, 0, 0, [r0 + view_w], .depth}

			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {.ctx, 0, r12, r12, r11}

			;darker colour
			s_call view, get_color, {.inst}, {r1}
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add r2, r1
			s_call gui_ctx, set_color, {.ctx, r1}

			;fill bottom edge and right
			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			s_call gui_ctx, filled_box, {.ctx, r12, r9, r10, r12}

			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {.ctx, r8, r9, r10, r11}
		else
			;sunken panel
			vp_mul -1, r12
			vp_cpy r12, .depth

			;darker colour
			s_call view, get_color, {.inst}, {r1}
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add r2, r1
			s_call gui_ctx, set_color, {.ctx, r1}

			;fill left edge and top
			vp_cpy .inst, r0
			vp_cpy .ctx, r0
			s_call gui_ctx, filled_box, {.ctx, 0, 0, [r0 + view_w], .depth}

			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {.ctx, 0, r12, r12, r11}

			;brighter colour
			s_call view, get_color, {.inst}, {r1}
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add 0x00808080, r2
			vp_add r2, r1
			s_call gui_ctx, set_color, {.ctx, r1}

			;fill bottom edge and right
			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			s_call gui_ctx, filled_box, {.ctx, r12, r9, r10, r12}

			vp_cpy .inst, r0
			vp_cpy .depth,r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {.ctx, r8, r9, r10, r11}
		endif

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
