%include 'inc/func.ninc'
%include 'class/class_stream_msg_out.ninc'

def_func class/stream_msg_out/deinit
	;inputs
	;r0 = stream_msg_out object
	;trashes
	;all but r0, r4

	ptr inst, msg

	push_scope
	retire {r0}, {inst}

	;wait for final ack
	func_call sys_mail, read, {&inst->stream_msg_out_ack_mailbox}, {msg}
	func_call sys_mem, free, {msg}

	expr {inst}, {r0}
	pop_scope
	return

def_func_end
