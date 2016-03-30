%ifndef APP_1234
%define APP_1234

	def_structure	sample_mail, ml_msg_header
		def_struct	sample_mail_reply_id, mailbox_id
		def_long	sample_mail_progress
		def_long	sample_mail_command
		def_long	sample_mail_task_count
	def_structure_end

%endif
