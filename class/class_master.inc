%ifndef CLASS_MASTER_1234
%define CLASS_MASTER_1234

%include 'class/class_ref.inc'

	def_class master, ref
	def_method create, class/master/create, static, {}, {r0}
	def_method new, class/master/new, static, {}, {r0}
	def_method init, class/master/init, static, {r0, r1}, {r1}
	def_method get_mailboxes, class/master/get_mailboxes, static, {r0}, {r1, r2}
	def_method start, class/master/start, static, {r0, r1, r2}
	def_method stop, class/master/stop, static, {r0}
	def_method get_state, class/master/get_state, static, {r0}, {r1}

	def_method deinit, class/master/deinit, override

	def_structure master, ref
		ptr master_input
		ptr master_output
		ptr master_error
		struct master_output_mailbox, mailbox
		struct master_error_mailbox, mailbox
		ulong master_state
	def_structure_end

%endif
