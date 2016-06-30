%ifndef APP_1234
%define APP_1234

	def_structure sample_msg, msg_header
		ulong sample_msg_index
		ulong sample_msg_task_count
		ulong sample_msg_mem_used
		offset sample_msg_reply_size
		ulong sample_msg_command
		struct sample_msg_reply_id, id
	def_structure_end

%endif
