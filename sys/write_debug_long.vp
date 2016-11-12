%include 'inc/func.ninc'
%include 'inc/syscall.ninc'
%include 'inc/string.ninc'

def_func sys/write_debug_long
	;inputs
	;r0 = function name
	;r1 = line number
	;r2 = debug string
	;r3 = debug int
	;trashes
	;r0-r3, r5

	def_struct local
		long local_name
		long local_line
		long local_string
		long local_long
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1, r2, r3
	set_dst [r4 + local_name], [r4 + local_line], [r4 + local_string], [r4 + local_long]
	map_src_to_dst

	f_call sys_io, string, {[r4 + local_name], 2}
	f_call sys_io, string, {"< ", 2}
	f_call sys_io, number, {[r4 + local_line], 2, 10}
	f_call sys_io, string, {" >: ", r1}
	f_call sys_io, string, {[r4 + local_string], 2}
	f_call sys_io, string, {" :-> 0x", 2}
	f_call sys_io, number, {[r4 + local_long], 2, 16}
	f_call sys_io, char, {10, 2}

	vp_add local_size, r4
	vp_ret

def_func_end
