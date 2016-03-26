%include 'inc/func.inc'
%include 'inc/gui.inc'
%include 'class/class_flow.inc'

	fn_function class/flow/pref_size
		;inputs
		;r0 = flow object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r0, r4

		def_structure	local
			def_long	local_w
			def_long	local_h
		def_structure_end

		vp_sub local_size, r4
		vp_cpy_cl 0, [r4 + local_w]
		vp_cpy_cl 0, [r4 + local_h]

		vp_cpy r4, r1
		vp_lea [rel callback], r2
		static_call flow, backward

		vp_cpy [r4 + local_w], r10
		vp_cpy [r4 + local_h], r11
		vp_add local_size, r4
		vp_ret

	callback:
		vp_push r1
		method_call view, pref_size
		vp_pop r1
		vp_cpy [r0 + view_parent], r2
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_up | flow_flag_down, r3
		if r3, !=, 0
			;flow down or up
			vp_add r11, [r1 + local_h]
		endif
		vp_cpy [r2 + flow_flags], r3
		vp_and flow_flag_left | flow_flag_right, r3
		if r3, !=, 0
			;flow left or right
			vp_add r10, [r1 + local_w]
		endif
		if r10, >, [r1 + local_w]
			vp_cpy r10, [r1 + local_w]
		endif
		if r11, >, [r1 + local_h]
			vp_cpy r11, [r1 + local_h]
		endif
		vp_ret

	fn_function_end
