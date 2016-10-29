%include 'inc/func.inc'
%include 'class/class_string.inc'

	def_func class/string/init
		;inputs
		;r0 = string object
		;r1 = vtable pointer
		;r2 = buffer pointer
		;r3 = buffer length
		;outputs
		;r1 = 0 if error, else ok

		def_struc local
			ptr local_inst
			long local_data
			long local_length
		def_struc_end

		;save inputs
		vp_sub local_size, r4
		set_src r2, r3
		set_dst [r4 + local_data], [r4 + local_length]
		map_src_to_dst

		;init parent
		s_call string, init, {r0, r1}, {r1}
		if r1, !=, 0
			vp_cpy r0, [r4 + local_inst]
			vp_xor r1, r1
			vp_cpy r1, [r0 + string_hashcode]

			;init myself
			vp_cpy [r4 + local_length], r2
			vp_cpy r2, [r0 + string_length]
			f_call sys_mem, copy, {[r4 + local_data], &[r0 + string_data], r2}, {_, r1}
			vp_xor r0, r0
			vp_cpy_ub r0, [r1]

			vp_cpy [r4 + local_inst], r0
		endif

		vp_add local_size, r4
		vp_ret

	def_func_end
