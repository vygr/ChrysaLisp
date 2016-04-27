%include 'inc/func.inc'
%include 'class/class_flow.inc'
%include 'class/class_label.inc'

	fn_function class/label/pref_size
		;inputs
		;r0 = label object
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

		method_call flow, pref_size, {[r0 + label_flow]}, {r10, r11}
		vp_add label_border_size * 2, r10
		vp_add label_border_size * 2, r11

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
