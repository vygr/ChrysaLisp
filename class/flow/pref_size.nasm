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

		def_local
			def_local_long	w
			def_local_long	h
		def_local_end

		vp_sub local_size, r4
		vp_xor r1, r1
		vp_cpy r1, .w
		vp_cpy r1, .h

		static_call flow, backward, {r0, r4, $callback}

		vp_cpy .w, r10
		vp_cpy .h, r11
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
