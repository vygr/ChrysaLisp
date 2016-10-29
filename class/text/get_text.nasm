%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	def_func class/text/get_text
		;inputs
		;r0 = text object
		;outputs
		;r0 = text object
		;r1 = 0 else, string object
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

		;reference the string
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			f_call string, ref, {r0}
		endif
		vp_cpy r0, r1

		vp_cpy [r4 + local_inst], r0
		vp_add local_size, r4
		vp_ret

	def_func_end
