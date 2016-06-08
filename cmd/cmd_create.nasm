%include 'inc/func.inc'
%include 'cmd/cmd.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'

	fn_function cmd/cmd_create
		;inputs
		;r0 = pipe master object
		;r1 = buffer
		;r2 = length
		;outputs
		;r0 = 0 if failed, else success
		;trashes
		;all but r4

		const pipe_char, '|'
		const space_char, ' '

		ptr pipe
		ptr buffer
		ulong length
		ulong index
		ptr msg
		ptr string
		ptr commands
		ptr args
		ptr stream
		pubyte start
		ptr ids
		struct nextid, id

		push_scope
		retire {r0, r1, r2}, {pipe, buffer, length}

		;split pipe into separate commands and args
;		static_call stream, create, {0, 0, buffer, length}, {stream}
		static_call stream, create, {0, 0, "cmd/forth a b c | cmd/forth q w e | cmd/forth z y x", 51}, {stream}
		static_call stream, split, {stream, pipe_char}, {args}
		static_call stream, deref, {stream}

		;create command pipeline
		static_call vector, get_length, {args}, {length}
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

		;send args to pipe elements, wiring up id's as we go
		assign {&pipe->cmd_master_output_mailbox}, {nextid.id_mbox}
		static_call sys_cpu, id, {}, {nextid.id_cpu}
		loop_while {index != 0}
			assign {index - 1}, {index}
			static_call sys_mail, alloc, {}, {msg}
			assign {(args->vector_array)[index * ptr_size]}, {string}
			assign {cmd_mail_init_size + string->string_length}, {msg->msg_length}
			static_call sys_mem, copy, {&string->string_data, &msg->cmd_mail_init_args, string->string_length}, {_, _}
			assign {nextid.id_mbox}, {msg->cmd_mail_init_stdout_id.id_mbox}
			assign {nextid.id_cpu}, {msg->cmd_mail_init_stdout_id.id_cpu}
			assign {&pipe->cmd_master_error_mailbox}, {msg->cmd_mail_init_stderr_id.id_mbox}
			static_call sys_cpu, id, {}, {msg->cmd_mail_init_stderr_id.id_cpu}
			assign {ids[index * id_size].id_mbox}, {nextid.id_mbox}
			assign {ids[index * id_size].id_cpu}, {nextid.id_cpu}
			assign {nextid.id_mbox}, {msg->msg_dest.id_mbox}
			assign {nextid.id_cpu}, {msg->msg_dest.id_cpu}
			static_call sys_mail, send, {msg}
		loop_end
		assign {nextid.id_mbox}, {pipe->cmd_master_input_id.id_mbox}
		assign {nextid.id_cpu}, {pipe->cmd_master_input_id.id_cpu}

		;wait for all acks
		loop_while {index != length}
			static_call sys_mail, read, {&pipe->cmd_master_error_mailbox}, {msg}
			static_call sys_mem, free, {msg}
			assign {index + 1}, {index}
		loop_end

		;free ids and args
		static_call vector, deref, {args}
		static_call sys_mem, free, {ids}

		;init seqnums
		assign {0, 0}, {pipe->cmd_master_input_seqnum, pipe->cmd_master_output_seqnum}

		;no error
		eval {1}, {r0}
		pop_scope
		vp_ret

	fn_function_end
