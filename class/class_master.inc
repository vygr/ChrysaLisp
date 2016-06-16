%ifndef CLASS_MASTER_1234
%define CLASS_MASTER_1234

%include 'class/class_ref.inc'

	def_class master, ref
	def_method create, class/master/create, static, {}, {r0}
	def_method new, class/master/new, static, {}, {r0}
	def_method init, class/master/init, static, {r0, r1}, {r1}
	def_method get_mailboxes, class/master/get_mailboxes, static, {r0}, {r1, r2}
	def_method input, class/master/input, static, {r0, r1, r2}
	def_method output, class/master/output, static, {r0}, {r1}
	def_method error, class/master/error, static, {r0}, {r1}
	def_method start, class/master/start, static, {r0, r1, r2}
	def_method stop, class/master/stop, static, {r0}
	def_method get_state, class/master/get_state, static, {r0}, {r1}
	def_method output_ready, class/master/output_ready, static, {r0}, {r1}

	def_method deinit, class/master/deinit, override

	def_structure master, ref
		struct master_input_id, id
		struct master_output_mailbox, mailbox
		struct master_error_mailbox, mailbox
		struct master_output_list, lh_list
		ulong master_input_seqnum
		ulong master_output_seqnum
		ulong master_state
	def_structure_end

%endif
