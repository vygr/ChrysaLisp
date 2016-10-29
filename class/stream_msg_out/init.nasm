%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'

def_func class/stream_msg_out/init
	;inputs
	;r0 = stream_msg_out object
	;r1 = vtable pointer
	;r2 = target mailbox id
	;r3 = target mailbox id
	;outputs
	;r1 = 0 if error, else ok
	;trashes
	;all but r0, r4

	;save inputs
	set_src r2, r3
	set_dst [r0 + stream_msg_out_id + id_mbox], [r0 + stream_msg_out_id + id_cpu]
	map_src_to_dst

	;init parent
	s_call stream_msg_out, init, {r0, r1, 0, 0, 0, 0}, {r1}
	if r1, !=, 0
		;init myself
		vp_cpy_cl 0, [r0 + stream_msg_out_seqnum]
		vp_cpy_cl 0, [r0 + stream_msg_out_ack_seqnum]
		vp_cpy_cl stream_mail_state_started, [r0 + stream_msg_out_state]
		vp_lea [r0 + stream_msg_out_ack_mailbox], r1
		ml_init r1, r2, r3
	endif
	vp_ret

def_func_end
