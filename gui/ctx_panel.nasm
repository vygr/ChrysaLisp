%include 'inc/func.inc'
%include 'inc/gui.inc'

	def_function gui/ctx_panel
		;inputs
		;r0 = ctx object
		;r1 = color
		;r2 = flags
		;r3 = depth
		;r8 = x
		;r9 = y
		;r10 = width
		;r11 = height
		;trashes
		;all r4

		def_structure local
			ptr local_ctx
			long local_color
			long local_flags
			long local_depth
			long local_x
			long local_y
			long local_w
			long local_h
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2, r3, r8, r9, r10, r11
		set_dst [r4 + local_ctx], [r4 + local_color], [r4 + local_flags], [r4 + local_depth], \
				[r4 + local_x], [r4 + local_y], [r4 + local_w], [r4 + local_h]
		map_src_to_dst

		if r2, !=, 0
			;fill middle
			s_call gui_ctx, set_color, {r0, r1}

			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_h], r11
			vp_cpy [r4 + local_depth],r12
			vp_add r12, r8
			vp_add r12, r9
			vp_shl 1, r12
			vp_sub r12, r10
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}
		endif

		vp_cpy [r4 + local_depth],r12
		if r12, >, 0
			;brighter colour
			s_call gui_ctx, brighter, {[r4 + local_color]}, {r1}
			s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

			;fill left edge and top
			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_depth],r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_depth],r10
			vp_cpy [r4 + local_h], r11
			vp_add r10, r9
			vp_sub r10, r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

			;darker colour
			s_call gui_ctx, darker, {[r4 + local_color]}, {r1}
			s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

			;fill bottom edge and right
			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_h], r11
			vp_cpy [r4 + local_depth],r12
			vp_add r12, r8
			vp_sub r12, r10
			vp_add r11, r9
			vp_sub r12, r9
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r12}

			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_h], r11
			vp_cpy [r4 + local_depth],r12
			vp_add r10, r8
			vp_sub r12, r8
			vp_add r12, r9
			vp_sub r12, r11
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r12, r11}
		else
			;sunken panel
			vp_mul -1, r12
			vp_cpy r12, [r4 + local_depth]

			;darker colour
			s_call gui_ctx, darker, {[r4 + local_color]}, {r1}
			s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

			;fill left edge and top
			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_depth],r11
			vp_sub r11, r10
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_depth],r10
			vp_cpy [r4 + local_h], r11
			vp_add r10, r9
			vp_sub r10, r11
			vp_sub r10, r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r11}

			;brighter colour
			s_call gui_ctx, brighter, {[r4 + local_color]}, {r1}
			s_call gui_ctx, set_color, {[r4 + local_ctx], r1}

			;fill bottom edge and right
			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_h], r11
			vp_cpy [r4 + local_depth],r12
			vp_add r11, r9
			vp_sub r12, r9
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r10, r12}

			vp_cpy [r4 + local_x], r8
			vp_cpy [r4 + local_y], r9
			vp_cpy [r4 + local_w], r10
			vp_cpy [r4 + local_h], r11
			vp_cpy [r4 + local_depth],r12
			vp_add r10, r8
			vp_sub r12, r8
			vp_sub r12, r11
			s_call gui_ctx, filled_box, {[r4 + local_ctx], r8, r9, r12, r11}
		endif

		vp_add local_size, r4
		vp_ret

	def_function_end
