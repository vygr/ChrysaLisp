%include 'inc/func.ninc'
%include 'class/class_stream_msg_in.ninc'
%include 'class/class_stream_msg_out.ninc'

def_func class/stream_msg_in/init
	;inputs
	;r0 = stream_msg_in object
	;r1 = vtable pointer
	;r2 = target mailbox
	;outputs
	;r1 = 0 if error, else ok
	;trashes
	;all but r0, r4

	;save inputs
	set_src r2
	set_dst [r0 + stream_msg_in_mailbox]
	map_src_to_dst

	;init parent
	s_call stream_msg_in, init, {r0, r1, 0, 0, 0, 0}, {r1}
	vpif r1, !=, 0
		;init myself
		vp_xor r1, r1
		vp_cpy r1, [r0 + stream_msg_in_seqnum]
		vp_cpy r1, [r0 + stream_msg_in_ack_seqnum]
		vp_cpy stream_mail_state_started, r1
		vp_cpy r1, [r0 + stream_msg_in_state]
		vp_lea [r0 + stream_msg_in_list], r1
		lh_init r1, r2
	endif
	vp_ret

def_func_end
