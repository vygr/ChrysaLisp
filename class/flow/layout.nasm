%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_flow.inc'

	fn_function class/flow/layout
		;inputs
		;r0 = flow object
		;trashes
		;all but r4

		def_structure	layout
			def_long	layout_x
			def_long	layout_y
		def_structure_end

		vp_sub layout_size, r4
		vp_cpy 0, qword[r4 + layout_x]
		vp_cpy 0, qword[r4 + layout_y]
		vp_cpy [r0 + flow_flags], r1
		vp_and flow_flag_left, r1
		if r1, !=, 0
			vp_cpy [r0 + view_w], r1
			vp_cpy r1, [r4 + layout_x]
		endif
		vp_cpy [r0 + flow_flags], r1
		vp_and flow_flag_up, r1
		if r1, !=, 0
			vp_cpy [r0 + view_h], r1
			vp_cpy r1, [r4 + layout_y]
		endif

		vp_cpy r4, r1
		vp_lea [rel callback], r2
		static_call flow, forward

		vp_add layout_size, r4
		vp_ret

	callback:
		vp_push r1, r0
		method_call view, pref_size

		vp_cpy [r4], r0
		vp_cpy [r4 + 8], r1
		vp_cpy [r1 + layout_x], r8
		vp_cpy [r1 + layout_y], r9
		vp_cpy r8, r12
		vp_cpy r9, r13

		vp_cpy [r0 + view_parent], r2
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_down, r3
		if r3, !=, 0
			;flow down
			vp_lea [r9 + r11], r13
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_up, r3
		if r3, !=, 0
			;flow up
			vp_sub r11, r9
			vp_cpy r9, r13
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_right, r3
		if r3, !=, 0
			;flow right
			vp_lea [r8 + r10], r12
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_left, r3
		if r3, !=, 0
			;flow left
			vp_sub r10, r8
			vp_cpy r8, r12
		endif
		vp_cpy r12, [r1 + layout_x]
		vp_cpy r13, [r1 + layout_y]
		static_call view, move

		vp_pop r1, r0
		vp_ret

	fn_function_end
