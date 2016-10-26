%include 'inc/func.inc'
%include 'class/class_master.inc'
%include 'class/class_stream_msg_out.inc'

	def_func class/master/init
		;inputs
		;r0 = master object
		;r1 = vtable pointer
		;outputs
		;r1 = 0 if error, else ok
		;trashes
		;all but r0, r4

		ptr inst, vtable
		ulong error

		;read init args
		push_scope
		retire {r0, r1}, {inst, vtable}

		;init parent
		super_call master, init, {inst, vtable}, {error}
		if {error != 0}
			;init state
			assign {stream_mail_state_stopped}, {inst->master_state}
		endif

		eval {inst, error}, {r0, r1}
		pop_scope
		return

	def_func_end
