%include 'inc/func.inc'
%include 'class/class_error.inc'
%include 'class/class_string.inc'

def_func class/error/init
	;inputs
	;r0 = error object
	;r1 = vtable pointer
	;r2 = cstr pointer
	;r3 = error payload object
	;outputs
	;r1 = 0 if error, else ok
	;trashes
	;r1-r3, r5-r7

	def_struct local
		ptr local_inst
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0, r2, r3
	set_dst [r4 + local_inst], [r0 + error_description], [r0 + error_object]
	map_src_to_dst

	;init parent
	s_call error, init, {r0, r1}, {r1}
	if r1, !=, 0
		;init self
		f_call string, create_from_cstr, {[r0 + error_description]}, {r0}
		vp_cpy [r4 + local_inst], r1
		vp_cpy r0, [r1 + error_description]
		f_call ref, ref, {[r1 + error_object]}
		vp_cpy [r4 + local_inst], r0
	endif

	vp_add local_size, r4
	vp_ret

def_func_end
