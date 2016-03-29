%ifndef APP_1234
%define APP_1234

	def_structure	app_mail, ml_msg_header
		def_struct	app_mail_reply_id, mailbox_id
		def_long	app_mail_progress
		def_long	app_mail_command
		def_long	app_mail_task_count
	def_structure_end

%endif
