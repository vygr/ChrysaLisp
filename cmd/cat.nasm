%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_slave.inc'

	fn_function cmd/cat

		buffer_size equ 120

		const char_lf, 10

		ptr slave
		ptr stream
		ptr arg
		ptr string
		ptr args
		pubyte charp
		ulong argc
		ulong index
		ulong length
		struct buffer, buffer

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave != 0}
			;cat arg files to stdout, arg 1 is command name
			static_call slave, get_args, {slave}, {args}
			static_call vector, get_length, {args}, {argc}
			assign {1}, {index}
			loop_while {index != argc}
				static_call vector, get_element, {args, index}, {arg}
				assign {index + 1}, {index}
				static_call string, create_from_file, {&arg->string_data}, {string}
				static_call string, deref, {arg}
				continueif {!string}
				static_call stream, create, {string, 0, &string->string_data, string->string_length}, {stream}
				loop_start
					static_call stream, read_line, {stream, &buffer, buffer_size - 1}, {length}
					breakif {length == -1}
					assign {&buffer + length}, {charp}
					assign {char_lf}, {*charp}
					static_call slave, stdout, {slave, &buffer, length + 1}
					static_call sys_task, yield
				loop_end
				static_call stream, deref, {stream}
			loop_end

			;clean up
			static_call slave, deref, {slave}
		endif
		pop_scope
		vp_ret

	fn_function_end
