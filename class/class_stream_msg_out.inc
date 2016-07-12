%ifndef CLASS_STREAM_MSG_OUT_1234
%define CLASS_STREAM_MSG_OUT_1234

%include 'class/class_stream.inc'

	def_class stream_msg_out, stream
	def_method create, class/stream_msg_out/create, static, {r0, r1}, {r0}
	def_method new, class/stream_msg_out/new, static, {}, {r0}
	def_method init, class/stream_msg_out/init, static, {r0, r1, r2, r3}, {r1}
	def_method next_seq, class/stream_msg_out/next_seq, static, {r0, r1, r2}, {r0}

	def_method deinit, class/stream_msg_out/deinit, override
	def_method write_flush, class/stream_msg_out/write_flush, override
	def_method write_next, class/stream_msg_out/write_next, override

	def_structure stream_msg_out, stream
		ulong stream_msg_out_seqnum
		ulong stream_msg_out_state
		struct stream_msg_out_id, id
		ulong stream_msg_out_ack_seqnum
		struct stream_msg_out_ack_mailbox, mailbox
	def_structure_end

	def_structure stream_mail, msg_header
		ulong stream_mail_seqnum
		ulong stream_mail_state
		struct stream_mail_ack_id, id
		offset stream_mail_data
	def_structure_end

	stream_msg_out_ack_shift equ 4

	def_enum
		enum stream_mail_state_started
		enum stream_mail_state_stopping
		enum stream_mail_state_stopped
	def_enum_end

%endif
