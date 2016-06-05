%ifndef APP_1234
%define APP_1234

	def_structure sample_mail, msg_header
		struct sample_mail_reply_id, id
		ptr sample_mail_progress
		ulong sample_mail_command
		ulong sample_mail_task_count
	def_structure_end

%endif
