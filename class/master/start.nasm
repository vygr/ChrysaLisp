%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_string.inc'
%include 'class/class_vector.inc'
%include 'class/class_master.inc'
%include 'class/class_slave.inc'

	fn_function class/master/start
		;inputs
		;r0 = master object
		;r1 = buffer
		;r2 = length
		;trashes
		;all but r0, r4

		const pipe_char, '|'
		const space_char, ' '

		ptr inst
		ptr buffer
		ulong length
		ulong index
		ulong started
		ptr msg
		ptr string
		ptr commands
		ptr args
		ptr stream
		pubyte start
		ptr ids
		struct nextid, id
		struct mailbox, mailbox

		;init vars
		push_scope
		retire {r0, r1, r2}, {inst, buffer, length}
		if {inst->master_state != master_state_started}
			;split pipe into separate commands and args
			static_call stream, create, {0, 0, buffer, length}, {stream}
			static_call stream, split, {stream, pipe_char}, {args}
			static_call stream, deref, {stream}
			static_call vector, get_length, {args}, {length}
			if {length != 0}
				;create command pipeline
				static_call vector, create, {}, {commands}
				assign {0}, {index}
				loop_while {index != length}
					assign {(args->vector_array)[index * ptr_size]}, {string}
					static_call stream, create, {0, 0, &string->string_data, string->string_length}, {stream}
					static_call stream, skip, {stream, space_char}
					assign {stream->stream_bufp}, {start}
					static_call stream, skip_not, {stream, space_char}
					static_call string, create_from_buffer, {start, stream->stream_bufp - start}, {string}
					static_call vector, push_back, {commands, string}
					static_call stream, deref, {stream}
					assign {index + 1}, {index}
				loop_end

				;open command pipeline
				static_call sys_task, open_pipe, {commands}, {ids}
				static_call vector, deref, {commands}

				;count how many started
				assign {0, 0}, {started, index}
				loop_while {index != length}
					if {ids[index * id_size].id_mbox != 0}
						assign {started + 1}, {started}
					endif
					assign {index + 1}, {index}
				loop_end

				;error if some didn't start
				if {started == length}
					;send args to inst elements, wiring up id's as we go
					static_call sys_mail, mailbox, {&mailbox}
					assign {&inst->master_output_mailbox}, {nextid.id_mbox}
					static_call sys_cpu, id, {}, {nextid.id_cpu}
					loop_while {index != 0}
						assign {index - 1}, {index}
						static_call sys_mail, alloc, {}, {msg}
						assign {(args->vector_array)[index * ptr_size]}, {string}
						assign {slave_mail_init_size + string->string_length}, {msg->msg_length}
						static_call sys_mem, copy, {&string->string_data, &msg->slave_mail_init_args, string->string_length}, {_, _}
						assign {nextid.id_mbox}, {msg->slave_mail_init_stdout_id.id_mbox}
						assign {nextid.id_cpu}, {msg->slave_mail_init_stdout_id.id_cpu}
						assign {&inst->master_error_mailbox}, {msg->slave_mail_init_stderr_id.id_mbox}
						static_call sys_cpu, id, {}, {msg->slave_mail_init_stderr_id.id_cpu}
						assign {&mailbox}, {msg->slave_mail_init_ack_id.id_mbox}
						static_call sys_cpu, id, {}, {msg->slave_mail_init_ack_id.id_cpu}
						assign {ids[index * id_size].id_mbox}, {nextid.id_mbox}
						assign {ids[index * id_size].id_cpu}, {nextid.id_cpu}
						assign {nextid.id_mbox}, {msg->msg_dest.id_mbox}
						assign {nextid.id_cpu}, {msg->msg_dest.id_cpu}
						static_call sys_mail, send, {msg}

						;wait for ack
						static_call sys_mail, read, {&mailbox}, {msg}
						static_call sys_mem, free, {msg}
					loop_end
					assign {nextid.id_mbox}, {inst->master_input_id.id_mbox}
					assign {nextid.id_cpu}, {inst->master_input_id.id_cpu}

					;init seqnums
					assign {0, 0}, {inst->master_input_seqnum, inst->master_output_seqnum}

					;no error
					assign {master_state_started}, {inst->master_state}
				else
					;send abort to any started pipe elements
					loop_while {index != 0}
						assign {index - 1}, {index}
						continueif {ids[index * id_size].id_mbox == 0}
						static_call sys_mail, alloc, {}, {msg}
						assign {msg_header_size}, {msg->msg_length}
						assign {ids[index * id_size].id_mbox}, {msg->msg_dest.id_mbox}
						assign {ids[index * id_size].id_cpu}, {msg->msg_dest.id_cpu}
						static_call sys_mail, send, {msg}
					loop_end
				endif

				;free ids
				static_call sys_mem, free, {ids}
			endif

			;free args
			static_call vector, deref, {args}
		endif

		eval {inst}, {r0}
		pop_scope
		return

	fn_function_end
