%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_msg_out.inc'

	fn_function class/master/select
		;inputs
		;r0 = master object
		;r1 = user mailbox
		;outputs
		;r0 = master object
		;r1 = mailbox with mail
		;trashes
		;all but r0, r4

		ptr inst
		ptr mailbox
		ulong length

		push_scope
		retire {r0, r1}, {inst, mailbox}

		if {inst->master_state != stream_mail_state_started}
			;not yet running, so just wait on user mailbox
			static_call sys_mail, select, {&mailbox, 1}, {mailbox}
		else
			;wait on user and pipe mailboxes
			assign {mailbox}, {*inst->master_select_array}
			static_call vector, get_length, {inst->master_streams}, {length}
			static_call sys_mail, select, {inst->master_select_array, length}, {mailbox}
		endif

		eval {inst, mailbox}, {r0, r1}
		pop_scope
		return

	fn_function_end
