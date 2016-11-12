%include 'inc/func.ninc'
%include 'class/class_string.ninc'
%include 'class/class_stream_str.ninc'

def_func class/stream_str/ref_string
	;inputs
	;r0 = stream_str object
	;outputs
	;r0 = stream_str object
	;r1 = string object
	;trashes
	;r2

	push r0
	vp_xor r2, r2
	vp_cpy [r0 + stream_bufp], r1
	vp_cpy_b r2, [r1]
	vp_cpy [r0 + stream_object], r0
	vp_lea [r0 + string_data], r2
	vp_sub r2, r1
	vp_cpy r1, [r0 + string_length]
	f_call string, ref, {r0}
	vp_cpy r0, r1
	pop r0
	vp_ret

def_func_end
