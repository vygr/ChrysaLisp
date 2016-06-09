%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_slave.inc'

	fn_function cmd/cat

		buffer_size equ 120

		ptr slave
		ptr arg
		ptr args
		ptr stream
		ulong argc
		ulong index
		ulong length
		struct buffer1, buffer
		struct buffer2, buffer

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave != 0}
			;cat files to stdout, arg 1 is command name
			static_call slave, get_args, {slave}, {args}
			static_call vector, get_length, {args}, {argc}
			if {argc != 1}
				;names from command line
				assign {1}, {index}
				loop_while {index != argc}
					static_call vector, get_element, {args, index}, {arg}
					local_call cat_string, {slave, arg, &buffer1}, {r0, r1, r2}
					assign {index + 1}, {index}
				loop_end
			else
				;names from stdin
				loop_start
					static_call slave, stdin, {slave, &buffer2, buffer_size - 1}, {length}
					breakif {!length}
					static_call stream, create, {0, 0, &buffer2, length}, {stream}
					loop_start
						static_call stream, read_line, {stream, &buffer1, buffer_size - 1}, {length}
						breakif {length == -1}
						static_call string, create_from_buffer, {&buffer1, length}, {arg}
						local_call cat_string, {slave, arg, &buffer1}, {r0, r1, r2}
					loop_end
					static_call stream, deref, {stream}
				loop_end
			endif

			;clean up
			static_call slave, deref, {slave}
		endif
		pop_scope
		vp_ret

	cat_string:
		;r0 = slave
		;r1 = arg string
		;r2 = buffer

		const char_lf, 10

		ptr slave
		ptr arg
		ptr buffer
		ptr string
		ptr stream
		pubyte charp
		ulong length

		push_scope
		retire {r0, r1, r2}, {slave, arg, buffer}

		static_call string, create_from_file, {&arg->string_data}, {string}
		static_call string, deref, {arg}
		if {string}
			static_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
			loop_start
				static_call stream, read_line, {stream, buffer, buffer_size - 1}, {length}
				breakif {length == -1}
				assign {buffer + length}, {charp}
				assign {char_lf}, {*charp}
				static_call slave, stdout, {slave, buffer, length + 1}
				static_call sys_task, yield
			loop_end
			static_call stream, deref, {stream}
		endif

		pop_scope
		vp_ret

	fn_function_end
