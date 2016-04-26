%include 'inc/func.inc'
%include 'class/class_window.inc'
%include 'class/class_flow.inc'

	fn_function class/window/pref_size
		;inputs
		;r0 = window object
		;outputs
		;r10 = prefered width
		;r11 = prefered height
		;trashes
		;all but r0, r4

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst .inst
		map_src_to_dst

		method_call flow, pref_size, {[r0 + window_flow]}, {r10, r11}
		vp_add window_border_size * 2, r10
		vp_add window_border_size * 2, r11

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
