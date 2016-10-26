%include 'inc/func.inc'
%include 'class/class_label.inc'
%include 'class/class_window.inc'

	def_func class/window/set_status
		;inputs
		;r0 = window object
		;r1 = 0, else status string object

		def_structure local
			ptr local_inst
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0
		set_dst [r4 + local_inst]
		map_src_to_dst

		f_call label, set_text, {[r0 + window_status], r1}
		f_call label, dirty, {r0}

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_func_end
