%include 'inc/func.ninc'
%include 'class/class_stream_msg_out.ninc'

def_func class/stream_msg_out/create
	;inputs
	;r0 = target mailbox id
	;r1 = target mailbox id
	;outputs
	;r0 = 0 if error, else object
	;trashes
	;r1-r3, r5-r6

	;create new stream_msg_out object
	set_src r0, r1
	set_dst r5, r6
	map_src_to_dst

	f_call stream_msg_out, new, {}, {r0}
	vpif r0, !=, 0
		;init the object
		func_path class, stream_msg_out
		f_call stream_msg_out, init, {r0, @_function_, r5, r6}, {r1}
		vpif r1, ==, 0
			;error with init
			v_call stream_msg_out, delete, {r0}, {}, r1
			vp_xor r0, r0
		endif
	endif
	vp_ret

def_func_end
