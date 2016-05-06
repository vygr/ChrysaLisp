%ifndef APP_1234
%define APP_1234

	def_structure	sample_mail, ml_msg_header
		struct	sample_mail_reply_id, mailbox_id
		long	sample_mail_progress
		long	sample_mail_command
		long	sample_mail_task_count
	def_structure_end

%endif
