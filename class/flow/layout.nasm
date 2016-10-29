%include 'inc/func.inc'
%include 'class/class_flow.inc'

	def_func class/flow/layout
		;inputs
		;r0 = flow object
		;trashes
		;all but r0, r4

		def_struct local
			long local_x
			long local_y
		def_struct_end

		vp_sub local_size, r4
		vp_xor r1, r1
		vp_cpy r1, [r4 + local_x]
		vp_cpy r1, [r4 + local_y]
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

		f_call flow, forward, {r0, r4, $callback}

		vp_add local_size, r4
		vp_ret

	callback:
		vp_push r1
		v_call view, pref_size, {r0}, {r10, r11}
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

		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_align_hcenter | flow_flag_align_hleft | flow_flag_align_hright, r3
		if r3, !=, 0
			vp_cpy [r2 + view_w], r8
			vp_sub r10, r8
			switch
			case r3, ==, flow_flag_align_hcenter
				vp_shr 1, r8
				break
			case r3, ==, flow_flag_align_hleft
				vp_xor r8, r8
			endswitch
		endif

		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_align_vcenter | flow_flag_align_vtop | flow_flag_align_vbottom, r3
		if r3, !=, 0
			vp_cpy [r2 + view_h], r9
			vp_sub r11, r9
			switch
			case r3, ==, flow_flag_align_vcenter
				vp_shr 1, r9
				break
			case r3, ==, flow_flag_align_vtop
				vp_xor r9, r9
			endswitch
		endif

		vp_cpy r12, [r1 + local_x]
		vp_cpy r13, [r1 + local_y]
		f_jmp view, change, {r0, r8, r9, r10, r11}

	def_func_end
