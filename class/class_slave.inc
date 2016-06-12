%ifndef CLASS_SLAVE_1234
%define CLASS_SLAVE_1234

%include 'class/class_ref.inc'

	def_class slave, ref
	def_method create, class/slave/create, static, {}, {r0}
	def_method new, class/slave/new, static, {}, {r0}
	def_method init, class/slave/init, static, {r0, r1}, {r1}
	def_method stdin, class/slave/stdin, static, {r0}, {r1, r2}
	def_method stdout, class/slave/stdout, static, {r0, r1, r2}
	def_method stderr, class/slave/stderr, static, {r0, r1, r2}
	def_method get_args, class/slave/get_args, static, {r0}, {r1}
	def_method next_seq, class/slave/next_seq, static, {r0, r1, r2}, {r0}

	def_method deinit, class/slave/deinit, override

	def_structure slave, ref
		struct slave_stdin_list, lh_list
		struct slave_stdout_id, id
		struct slave_stderr_id, id
		ulong slave_stdout_seqnum
		ulong slave_stdin_seqnum
		ptr slave_args
	def_structure_end

	def_structure slave_mail_init, msg_header
		struct slave_mail_init_stdout_id, id
		struct slave_mail_init_stderr_id, id
		struct slave_mail_init_ack_id, id
		offset slave_mail_init_args
	def_structure_end

	def_structure slave_mail_stream, msg_header
		ulong slave_mail_stream_seqnum
		ulong slave_mail_stream_state
		offset slave_mail_stream_data
	def_structure_end

%endif
