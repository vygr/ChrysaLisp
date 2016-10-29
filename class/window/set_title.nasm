%include 'inc/func.inc'
%include 'class/class_title.inc'
%include 'class/class_window.inc'

def_func class/window/set_title
	;inputs
	;r0 = window object
	;r1 = 0, else title string object

	def_struct local
		ptr local_inst
	def_struct_end

	;save inputs
	vp_sub local_size, r4
	set_src r0
	set_dst [r4 + local_inst]
	map_src_to_dst

	f_call title, set_text, {[r0 + window_title], r1}

	vp_cpy [r4 + local_inst], r0
	vp_add local_size, r4
	vp_ret

def_func_end
