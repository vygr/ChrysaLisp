%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_label.inc'

	fn_function class/label/set_text
		;inputs
		;r0 = label object
		;r1 = 0, else string pointer
		;trashes
		;all but r0, r4

		def_structure local
			long local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		s_call text, set_text, {[r0 + label_text], r1}

		vp_cpy [r4 + local_inst], r0
		m_call label, layout, {[r0 + label_flow]}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
