%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/get_text
		;inputs
		;r0 = label object
		;outputs
		;r0 = label object
		;r1 = 0, else string object

		def_local
			def_local_long	inst
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst .inst
		map_src_to_dst

		s_call text, get_text, {[r0 + label_text]}, {r1}

		vp_cpy .inst, r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
