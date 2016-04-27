%include 'inc/func.inc'
%include 'inc/syscall.inc'
%include 'inc/string.inc'

	fn_function sys/write_debug_long, no_debug_enter
		;inputs
		;r0 = function name
		;r1 = line number
		;r2 = debug string
		;r3 = debug int
		;trashes
		;r0-r3, r5

		def_local
			def_local_long	name
			def_local_long	line
			def_local_long	string
			def_local_long	long
		def_local_end

		;save inputs
		vp_sub local_size, r4
		set_src r0, r1, r2, r3
		set_dst .name, .line, .string, .long
		map_src_to_dst

		;tab in by stack depth
		static_call sys_task, stack_depth, {}, {r0}
		if r0, >, 0
			vp_lea [r0 - (12*8)], r2
			vp_cpy ' ', r0
			vp_cpy 2, r1
			loop_while r2, >, 0
				static_call sys_io, char
				vp_sub 8, r2
			loop_end
		endif
		static_call sys_io, string, {.name, 2}
		static_call sys_io, string, {"< ", 2}
		static_call sys_io, number, {.line, 2, 10}
		static_call sys_io, string, {" >: ", r1}
		static_call sys_io, string, {.string, 2}
		static_call sys_io, string, {" :-> 0x", 2}
		static_call sys_io, number, {.long, 2, 16}
		static_call sys_io, char, {10, 2}

		vp_add local_size, r4
		vp_ret

	fn_function_end
