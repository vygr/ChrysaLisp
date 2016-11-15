%include 'inc/func.ninc'
%include 'class/class_flow.ninc'

def_func class/flow/pref_size
	;inputs
	;r0 = flow object
	;outputs
	;r10 = prefered width
	;r11 = prefered height
	;trashes
	;all but r0, r4

	def_struct local
		long local_w
		long local_h
	def_struct_end

	vp_sub local_size, r4
	vp_xor r1, r1
	vp_cpy r1, [r4 + local_w]
	vp_cpy r1, [r4 + local_h]

	f_call flow, forward, {r0, r4, $callback}

	vp_cpy [r4 + local_w], r10
	vp_cpy [r4 + local_h], r11
	vp_add local_size, r4
	vp_ret

callback:
	vp_push r1
	v_call view, pref_size, {r0}, {r10, r11}
	vp_pop r1
	vp_cpy [r0 + view_parent], r2
	vp_cpy [r2 + flow_flags], r3
	vp_and flow_flag_up | flow_flag_down, r3
	vpif r3, !=, 0
		;flow down or up
		vp_cpy [r1 + local_h], r3
		vp_add r11, r3
		vp_cpy r3, [r1 + local_h]
	endif
	vp_cpy [r2 + flow_flags], r3
	vp_and flow_flag_left | flow_flag_right, r3
	vpif r3, !=, 0
		;flow left or right
		vp_cpy [r1 + local_w], r3
		vp_add r10, r3
		vp_cpy r3, [r1 + local_w]
	endif
	vpif r10, >, [r1 + local_w]
		vp_cpy r10, [r1 + local_w]
	endif
	vpif r11, >, [r1 + local_h]
		vp_cpy r11, [r1 + local_h]
	endif
	vp_ret

def_func_end
