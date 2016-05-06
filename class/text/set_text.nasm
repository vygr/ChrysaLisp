%include 'inc/func.inc'
%include 'class/class_text.inc'
%include 'class/class_string.inc'

	fn_function class/text/set_text
		;inputs
		;r0 = text object
		;r1 = 0, else string object
		;trashes
		;all but r0, r4

		def_structure local
			long local_inst
			long local_string
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1
		set_dst [r4 + local_inst], [r4 + local_string]
		map_src_to_dst

		;deref the old string
		vp_cpy [r0 + text_string], r0
		if r0, !=, 0
			s_call string, deref, {r0}
		endif

		;transfer reference to new string
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r4 + local_string], r1
		vp_cpy r1, [r0 + text_string]

		vp_add local_size, r4
		vp_ret

	fn_function_end
