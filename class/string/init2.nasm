%include 'inc/func.ninc'
%include 'inc/syscall.ninc'
%include 'class/class_string.ninc'

def_func class/string/init2
	;inputs
	;r0 = string object
	;r1 = vtable pointer
	;r2 = filename
	;r3 = file length
	;outputs
	;r1 = 0 if error, else ok

	def_struct local
		ptr local_inst
		long local_filename
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r2, r3
	set_dst [r4 + local_filename], [r0 + string_length]
	map_src_to_dst

	;init parent
	s_call string, init, {r0, r1}, {r1}
	vpif r1, !=, 0
		;init myself
		vp_cpy r0, [r4 + local_inst]
		vp_xor r1, r1
		vp_cpy r1, [r0 + string_hashcode]

		;open file
		vp_cpy [r4 + local_filename], r0
		sys_open r0, o_rdonly, 0
		vp_cpy r0, r7

		;read into buffer
		vp_cpy [r4 + local_inst], r0
		vp_cpy [r0 + string_length], r1
		vp_add string_data, r0
		vp_xor r2, r2
		vp_cpy_b r2, [r0 + r1]
		sys_read r7, r0, r1

		;close file
		sys_close r7

		vp_cpy [r4 + local_inst], r0
		vp_cpy r0, r1
	endif
	vp_add local_size, r4
	vp_ret

def_func_end
