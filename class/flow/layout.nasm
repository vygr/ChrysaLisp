%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_flow.inc'

	fn_function class/flow/layout
		;inputs
		;r0 = flow object
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_x
			def_long	local_y
		def_structure_end

		vp_sub local_size, r4
		vp_cpy_cl 0, [r4 + local_x]
		vp_cpy_cl 0, [r4 + local_y]
		vp_cpy [r0 + flow_flags], r1
		vp_and flow_flag_left, r1
		if r1, !=, 0
			vp_cpy [r0 + view_w], r1
			vp_cpy r1, [r4 + local_x]
		endif
		vp_cpy [r0 + flow_flags], r1
		vp_and flow_flag_up, r1
		if r1, !=, 0
			vp_cpy [r0 + view_h], r1
			vp_cpy r1, [r4 + local_y]
		endif

		vp_cpy r4, r1
		vp_lea [rel callback], r2
		static_call flow, backward

		vp_add local_size, r4
		vp_ret

	callback:
		vp_push r1
		method_call view, pref_size
		vp_pop r1
		vp_cpy [r1 + local_x], r8
		vp_cpy [r1 + local_y], r9
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
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_fillw, r3
		if r3, !=, 0
			;fill width of parent
			vp_cpy [r2 + view_w], r10
			vp_xor r8, r8
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_fillh, r3
		if r3, !=, 0
			;fill height of parent
			vp_cpy [r2 + view_h], r11
			vp_xor r9, r9
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_lastw, r3
		if r3, !=, 0
			;last one fills width gap of parent
			ln_is_last r0 + view_node, r3
			if r3, ==, 0
				vp_cpy [r2 + flow_flags], r3
				vp_and flow_flag_right, r3
				if r3, !=, 0
					;flow right
					vp_lea [r8 + r10], r15
					vp_sub [r2 + view_w], r15
					vp_sub r15, r10
				endif
				vp_cpy [r2 + flow_flags], r3
				vp_and flow_flag_left, r3
				if r3, !=, 0
					;flow left
					vp_add r8, r10
					vp_xor r8, r8
				endif
			endif
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_lasth, r3
		if r3, !=, 0
			;last one fills height gap of parent
			ln_is_last r0 + view_node, r3
			if r3, ==, 0
				vp_cpy [r2 + flow_flags], r3
				vp_and flow_flag_down, r3
				if r3, !=, 0
					;flow down
					vp_lea [r9 + r11], r15
					vp_sub [r2 + view_h], r15
					vp_sub r15, r11
				endif
				vp_cpy [r2 + flow_flags], r3
				vp_and flow_flag_up, r3
				if r3, !=, 0
					;flow up
					vp_add r9, r11
					vp_xor r9, r9
				endif
			endif
		endif
		vp_cpy r12, [r1 + local_x]
		vp_cpy r13, [r1 + local_y]
		static_call view, change
		vp_ret

	fn_function_end
