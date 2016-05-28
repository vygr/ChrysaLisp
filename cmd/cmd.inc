%ifndef CMD_1234
%define CMD_1234

	def_class cmd
	def_method init, cmd/cmd_init, static, {r0}
	def_method next_msg, cmd/cmd_next_msg, static, {r0, r1, r2}, {r0}
	def_method stdin, cmd/cmd_stdin, static, {r0, r1, r2}, {r0}
	def_method stdout, cmd/cmd_stdout, static, {r0, r1, r2}
	def_method stderr, cmd/cmd_stderr, static, {r0, r1, r2}

	def_structure cmd_mail_init, ml_msg_header
		struct cmd_mail_init_stdout_id, mailbox_id
		struct cmd_mail_init_stderr_id, mailbox_id
		struct cmd_mail_init_args, null
	def_structure_end

	def_structure cmd_mail_stream, ml_msg_header
		ulong cmd_mail_stream_seqnum
		struct cmd_mail_stream_data, null
	def_structure_end

	cmd_pipe_buffer_size equ 120

	def_structure cmd_pipe
		struct cmd_pipe_stdin_list, lh_list
		struct cmd_pipe_stdout_id, mailbox_id
		struct cmd_pipe_stderr_id, mailbox_id
		ulong cmd_pipe_stdout_seqnum
		ulong cmd_pipe_stdin_seqnum
		struct cmd_pipe_args, cmd_pipe_buffer
	def_structure_end

%endif
