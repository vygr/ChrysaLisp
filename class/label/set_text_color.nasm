%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	def_func class/label/set_text_color
		;inputs
		;r0 = label object
		;r1 = color
		;trashes
		;all but r0, r4

		def_struct local
			ptr local_inst
		def_struct_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		f_call text, set_text_color, {[r0 + label_text], r1}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_func_end
