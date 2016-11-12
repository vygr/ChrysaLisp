%include 'inc/func.ninc'
%include 'class/class_stream_msg_out.ninc'

def_func class/stream_msg_out/next_seq
	;inputs
	;r0 = list head
	;r1 = 0, else new stream msg
	;r2 = -1 or seqnum
	;outputs
	;r0 = 0, else next stream msg

	;save any new msg
	if r1, !=, 0
		lh_add_at_tail r0, r1, r3
	endif

	;scan for seqnum
	loop_list_forward r0, r0, r1
		breakif r2, ==, -1
	loop_until r2, ==, [r0 + stream_mail_seqnum]
	if r1, ==, 0
		;not found
		vp_cpy r1, r0
	else
		;remove found from list
		vp_cpy r0, r1
		ln_remove_node r1, r2
	endif
	vp_ret

def_func_end
