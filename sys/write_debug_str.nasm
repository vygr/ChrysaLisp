%include 'inc/func.inc'
%include 'inc/syscall.inc'
%include 'inc/string.inc'

	fn_function sys/write_debug_str, no_debug_enter
		;inputs
		;r0 = function name
		;r1 = line number
		;r2 = debug string
		;r3 = debug string
		;trashes
		;r0-r3, r5

		def_structure local
			long local_name
			long local_line
			long local_string
			long local_str
		def_structure_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2, r3
		set_dst [r4 + local_name], [r4 + local_line], [r4 + local_string], [r4 + local_str]
		map_src_to_dst

		;tab in by stack depth
		s_call sys_task, stack_depth, {}, {r0}
		if r0, >, 0
			vp_lea [r0 - (12*8)], r2
			vp_cpy ' ', r0
			vp_cpy 2, r1
			loop_while r2, >, 0
				s_call sys_io, char, {r0, r1}
				vp_sub 8, r2
			loop_end
		endif
		s_call sys_io, string, {[r4 + local_name], 2}
		s_call sys_io, string, {"< ", 2}
		s_call sys_io, number, {[r4 + local_line], 2, 10}
		s_call sys_io, string, {" >: ", r1}
		s_call sys_io, string, {[r4 + local_string], 2}
		s_call sys_io, string, {" :-> ", 2}
		s_call sys_io, string, {[r4 + local_str], 2}
		s_call sys_io, char, {10, 2}

		vp_add local_size, r4
		vp_ret

	fn_function_end
