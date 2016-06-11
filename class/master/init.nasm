%include 'inc/func.inc'
%include 'class/class_master.inc'

	fn_function class/master/init
		;inputs
		;r0 = master object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		ptr inst
		ptr vtable
		ulong error

		;read init args
		push_scope
		retire {r0, r1}, {inst, vtable}

		;init parent
		super_call master, init, {inst, vtable}, {error}
		if {error != 0}
			;init order lists
			static_call sys_list, init, {&inst->master_output_list}

			;init output and error mailboxes
			static_call sys_mail, mailbox, {&inst->master_output_mailbox}
			static_call sys_mail, mailbox, {&inst->master_error_mailbox}

			;init state
			assign {master_state_stopped}, {inst->master_state}
		endif

		eval {inst, error}, {r0, r1}
		pop_scope
		return

	fn_function_end
