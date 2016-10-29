%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	def_func class/label/set_text
		;inputs
		;r0 = label object
		;r1 = 0, else string pointer
		;trashes
		;all but r0, r4

		def_struc local
			ptr local_inst
		def_struc_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		f_call text, set_text, {[r0 + label_text], r1}

		vp_cpy [r4 + local_inst], r0
		v_call label, layout, {[r0 + label_flow]}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_func_end
