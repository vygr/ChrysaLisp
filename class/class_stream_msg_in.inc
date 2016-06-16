%ifndef CLASS_STREAM_MSG_IN_1234
%define CLASS_STREAM_MSG_IN_1234

%include 'class/class_stream.inc'

	def_class stream_msg_in, stream
	def_method create, class/stream_msg_in/create, static, {r0}, {r0}
	def_method new, class/stream_msg_in/new, static, {}, {r0}
	def_method init, class/stream_msg_in/init, static, {r0, r1, r2}, {r1}

	def_method read_next, class/stream_msg_in/read_next, override

	def_structure stream_msg_in, stream
		ulong stream_msg_in_seqnum
		ulong stream_msg_in_state
		ptr stream_msg_in_mailbox
		struct stream_msg_in_list, lh_list
	def_structure_end

%endif
