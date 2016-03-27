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

		def_structure	local
			def_long	local_inst
			def_long	local_ctx
			def_long	local_flags
			def_long	local_depth
		def_structure_end

		vp_sub local_size, r4
		vp_cpy r0, [r4 + local_inst]
		vp_cpy r1, [r4 + local_ctx]
		vp_cpy r2, [r4 + local_flags]
		vp_cpy r3, [r4 + local_depth]

		if r2, !=, 0
			;fill middle
			static_call view, get_color
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, set_color

			vp_cpy [r4 + local_depth],r12
			vp_cpy [r4 + local_inst], r0
			vp_cpy r12, r8
			vp_cpy r12, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r10
			vp_sub r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box
		endif

		vp_cpy [r4 + local_depth],r12
		if r12, >, 0
			;brighter colour
			vp_cpy [r4 + local_inst], r0
			static_call view, get_color
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add 0x00808080, r2
			vp_add r2, r1
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, set_color

			;fill left edge and top
			vp_cpy [r4 + local_inst], r0
			vp_xor r8, r8
			vp_xor r9, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r4 + local_depth], r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_xor r8, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			;darker colour
			vp_cpy [r4 + local_inst], r0
			static_call view, get_color
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add r2, r1
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, set_color

			;fill bottom edge and right
			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_cpy r12, r8
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			vp_cpy r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box
		else
			;sunken panel
			vp_mul -1, r12
			vp_cpy r12, [r4 + local_depth]

			;darker colour
			vp_cpy [r4 + local_inst], r0
			static_call view, get_color
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add r2, r1
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, set_color

			;fill left edge and top
			vp_cpy [r4 + local_inst], r0
			vp_xor r8, r8
			vp_xor r9, r9
			vp_cpy [r0 + view_w], r10
			vp_cpy [r4 + local_depth], r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_xor r8, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_sub r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			;brighter colour
			vp_cpy [r4 + local_inst], r0
			static_call view, get_color
			vp_cpy r1, r2
			vp_cpy 0xff000000, r3
			vp_and r3, r1
			vp_and 0x00fefefe, r2
			vp_shr 1, r2
			vp_add 0x00808080, r2
			vp_add r2, r1
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, set_color

			;fill bottom edge and right
			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_cpy r12, r8
			vp_cpy [r0 + view_h], r9
			vp_sub r12, r9
			vp_cpy [r0 + view_w], r10
			vp_sub r12, r10
			vp_cpy r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box

			vp_cpy [r4 + local_inst], r0
			vp_cpy [r4 + local_depth],r12
			vp_cpy [r0 + view_w], r8
			vp_sub r12, r8
			vp_cpy r12, r9
			vp_cpy r12, r10
			vp_cpy [r0 + view_h], r11
			vp_shl 1, r12
			vp_sub r12, r11
			vp_cpy [r4 + local_ctx], r0
			static_call gui_ctx, filled_box
		endif

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
