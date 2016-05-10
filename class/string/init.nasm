%include 'inc/func.inc'
%include 'class/class_string.inc'

	fn_function class/string/init
		;inputs
		;r0 = string object
		;r1 = vtable pointer
		;r2 = c string pointer
		;r3 = c string length
		;outputs
		;r1 = 0 if error, else ok

		def_structure local
			long local_inst
			long local_data
			long local_length
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r2, r3
		set_dst [r4 + local_data], [r4 + local_length]
		map_src_to_dst

		;init parent
		p_call string, init, {r0, r1, r2, r3}, {r1}
		if r1, !=, 0
			vp_cpy r0, [r4 + local_inst]

			;init myself
			vp_cpy [r4 + local_length], r2
			vp_cpy r2, [r0 + string_length]
			vp_inc r2
			s_call sys_mem, copy, {[r4 + local_data], &[r0 + string_data], r2}, {_, _}

			vp_cpy [r4 + local_inst], r0
		endif

		vp_add local_size, r4
		vp_ret

	fn_function_end
