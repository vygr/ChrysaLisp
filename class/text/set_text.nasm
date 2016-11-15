%include 'inc/func.ninc'
%include 'class/class_text.ninc'
%include 'class/class_string.ninc'

def_func class/text/set_text
	;inputs
	;r0 = text object
	;r1 = 0, else string object
	;trashes
	;all but r0, r4

	def_struct local
		ptr local_inst
		long local_string
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1
	set_dst [r4 + local_inst], [r4 + local_string]
	map_src_to_dst

	;deref the old string and words
	vp_cpy [r0 + text_string], r0
	vpif r0, !=, 0
		f_call string, deref, {r0}
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r0 + text_words], r1
		vp_xor r2, r2
		vp_cpy r2, [r0 + text_words]
		f_call string, deref, {r1}
	endif

	;transfer reference to new string
	vp_cpy [r4 + local_inst], r0
	vp_cpy [r4 + local_string], r1
	vp_cpy r1, [r0 + text_string]
	vpif r1, !=, 0
		f_call string, split, {r1, 32}, {r1}
		vp_cpy [r4 + local_inst], r0
		vp_cpy r1, [r0 + text_words]
	endif

	vp_add local_size, r4
	vp_ret

def_func_end
