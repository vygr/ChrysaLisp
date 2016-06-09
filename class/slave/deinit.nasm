%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_master.inc'
%include 'class/class_vector.inc'

	fn_function class/slave/deinit
		;inputs
		;r0 = slave object
		;trashes
		;all but r0, r4

		ptr inst
		ptr msg

		push_scope
		retire {r0}, {inst}

		;send normal EOF
		static_call slave, stdout, {inst, 0, 0}

		;wait for master EOF
		loop_start
			assign {0}, {msg}
			loop_start
				static_call slave, next_seq, {&inst->slave_stdin_list, msg, inst->slave_stdin_seqnum}, {msg}
				breakif {msg}
				static_call sys_mail, mymail, {}, {msg}
			loop_end
			assign {inst->slave_stdin_seqnum + 1}, {inst->slave_stdin_seqnum}
			breakif {msg->slave_mail_stream_state == master_state_stopped}
			static_call sys_mem, free, {msg}
		loop_end

		;forward master EOF to next
		assign {inst->slave_stdout_id.id_mbox}, {msg->msg_dest.id_mbox}
		assign {inst->slave_stdout_id.id_cpu}, {msg->msg_dest.id_cpu}
		assign {inst->slave_stdout_seqnum}, {msg->slave_mail_stream_seqnum}
		static_call sys_mail, send, {msg}

		;free args
		static_call vector, deref, {inst->slave_args}

		eval {inst}, {r0}
		pop_scope

		;deinit parent
		p_jmp slave, deinit, {r0}

	fn_function_end
