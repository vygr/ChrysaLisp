%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_window.inc'

	fn_function class/window/set_status
		;inputs
		;r0 = window object
		;r1 = 0, else status string object

		def_structure local
			long local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		s_call label, set_text, {[r0 + window_status], r1}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	fn_function_end
