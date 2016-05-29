%include 'inc/func.inc'
%include 'inc/string.inc'
%include 'inc/mail.inc'
%include 'cmd/cmd.inc'

	fn_function cmd/cmd_master
		;inputs
		;r0 = pipe struct pointer
		;trashes
		;all but r4

		ptr pipe

		;read init args
		push_scope
		retire {r0}, {pipe}

		;init seqnums
		assign {0, 0}, {pipe->cmd_master_input_seqnum, pipe->cmd_master_output_seqnum}

		;init order lists
		static_call sys_list, init, {&pipe->cmd_master_output_list}

		;init output and error mailboxes
		static_call sys_mail, mailbox, {&pipe->cmd_master_output_mailbox}
		static_call sys_mail, mailbox, {&pipe->cmd_master_error_mailbox}

		pop_scope
		vp_ret

	fn_function_end
