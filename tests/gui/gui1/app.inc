%ifndef APP_1234
%define APP_1234

	def_structure sample_msg, msg_header
		ptr sample_msg_progress
		ulong sample_msg_task_count
		struct sample_msg_reply_size, null
		ulong sample_msg_command
		struct sample_msg_reply_id, id
	def_structure_end

%endif
