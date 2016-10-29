%include 'inc/func.inc'
%include 'inc/syscall.inc'
%include 'inc/string.inc'

def_func sys/write_debug_str
	;inputs
	;r0 = function name
	;r1 = line number
	;r2 = debug string
	;r3 = debug string
	;trashes
	;r0-r3, r5

	def_struc local
		long local_name
		long local_line
		long local_string
		long local_str
	def_struc_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r1, r2, r3
	set_dst [r4 + local_name], [r4 + local_line], [r4 + local_string], [r4 + local_str]
	map_src_to_dst

	f_call sys_io, string, {[r4 + local_name], 2}
	f_call sys_io, string, {"< ", 2}
	f_call sys_io, number, {[r4 + local_line], 2, 10}
	f_call sys_io, string, {" >: ", r1}
	f_call sys_io, string, {[r4 + local_string], 2}
	f_call sys_io, string, {" :-> ", 2}
	f_call sys_io, string, {[r4 + local_str], 2}
	f_call sys_io, char, {10, 2}

	vp_add local_size, r4
	vp_ret

def_func_end
