%include 'inc/func.ninc'
%include 'class/class_stream_msg_out.ninc'

def_func class/stream_msg_out/set_state
	;inputs
	;r0 = stream_msg_out object
	;r1 = stream state

	vp_cpy r1, [r0 + stream_msg_out_state]
	vp_ret

def_func_end
