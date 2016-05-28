%ifndef CMD_1234
%define CMD_1234

	def_class cmd
	def_method next_msg, cmd/cmd_next_msg, static, {r0, r1, r2}, {r0}

	def_structure cmd_mail, ml_msg_header
		struct cmd_mail_reply_id, mailbox_id
		struct cmd_mail_stdout_id, mailbox_id
		struct cmd_mail_stderr_id, mailbox_id
		ulong cmd_mail_seqnum
		struct cmd_mail_string, null
	def_structure_end

%endif
