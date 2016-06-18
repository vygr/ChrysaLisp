%include 'inc/func.inc'
%include 'class/class_stream_msg_out.inc'
%include 'class/class_stream_msg_in.inc'
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
		ptr mbox
		pubyte start
		ptr ids
		struct nextid, id
		struct mailbox, mailbox

		;init vars
		push_scope
		retire {r0, r1, r2}, {inst, buffer, length}
		if {inst->master_state != stream_mail_state_started}
			;split pipe into separate commands and args
			static_call stream, create, {0, 0, buffer, length}, {stream}
			static_call stream, split, {stream, pipe_char}, {args}
			static_call stream, deref, {stream}
			static_call vector, get_length, {args}, {length}
			if {length != 0}
				;create command pipeline
				static_call vector, create, {}, {commands}
				static_call vector, set_capacity, {commands, length}
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
					;create streams, mailboxes and select array
					static_call vector, create, {}, {inst->master_streams}
					static_call vector, set_capacity, {inst->master_streams, length + 2}
					static_call sys_mem, alloc, {(length + 2) * ptr_size}, {inst->master_select_array, _}
					static_call sys_mem, alloc, {(length + 2) * mailbox_size}, {inst->master_mailbox_array, _}
					assign {0}, {index}
					loop_while {index != (length + 2)}
						assign {&(inst->master_mailbox_array)[index * mailbox_size]}, {mbox}
						static_call sys_mail, init_mailbox, {mbox}
						assign {mbox}, {(inst->master_select_array)[index * ptr_size]}
						static_call stream_msg_in, create, {mbox}, {stream}
						static_call vector, push_back, {inst->master_streams, stream}
						assign {index + 1}, {index}
					loop_end

					;send args to inst elements, wiring up id's as we go
					static_call sys_mail, init_mailbox, {&mailbox}
					assign {mbox}, {nextid.id_mbox}
					static_call sys_cpu, id, {}, {nextid.id_cpu}
					assign {index - 2}, {index}
					loop_while {index != 0}
						assign {index - 1}, {index}
						assign {(args->vector_array)[index * ptr_size]}, {string}
						static_call sys_mail, alloc_parcel, {slave_mail_init_size + string->string_length}, {msg}
						static_call sys_mem, copy, {&string->string_data, &msg->slave_mail_init_args, string->string_length}, {_, _}
						assign {nextid.id_mbox}, {msg->slave_mail_init_stdout_id.id_mbox}
						assign {nextid.id_cpu}, {msg->slave_mail_init_stdout_id.id_cpu}
						assign {&(inst->master_mailbox_array)[(index + 1) * mailbox_size]}, {msg->slave_mail_init_stderr_id.id_mbox}
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

					;create input stream, free the unused space keeper stream
					assign {*inst->master_streams->vector_array}, {stream}
					static_call stream_msg_in, deref, {stream}
					static_call stream_msg_out, create, {nextid.id_mbox, nextid.id_cpu}, {stream}
					assign {stream}, {*inst->master_streams->vector_array}

					;no error
					assign {stream_mail_state_started}, {inst->master_state}
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
