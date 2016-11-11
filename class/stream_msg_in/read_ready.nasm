%include 'inc/func.ninc'
%include 'class/class_stream_msg_in.ninc'

def_func class/stream_msg_in/read_ready
	;inputs
	;r0 = stream_msg_in object
	;outputs
	;r0 = stream_msg_in object
	;r1 = 0 if data not available
	;trashes
	;all but r0, r4

	;extend test to include mailbox
	s_call stream_msg_in, read_ready, {r0}, {r1}
	if r1, ==, 0
		vp_cpy [r0 + stream_msg_in_list + lh_list_head], r1
		vp_cpy [r1 + ln_node_succ], r1
	endif
	vp_ret

def_func_end
