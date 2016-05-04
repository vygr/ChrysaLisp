%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_text_color
		;inputs
		;r0 = label object
		;r1 = color
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

		s_call text, set_text_color, {[r0 + label_text], r1}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
