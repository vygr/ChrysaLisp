%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_slave.inc'

	def_function cmd/echo

		const char_lf, 10

		ptr slave, arg, args, stream
		ulong argc, index, length

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;echo args to stdout, arg 1 is command name
			static_call slave, get_args, {slave}, {args}
			slot_call vector, get_length, {args}, {argc}
			if {argc != 1}
				;strings from command line
				assign {1}, {index}
				loop_while {index != argc}
					static_call vector, get_element, {args, index}, {arg}
					static_call stream, write, {slave->slave_stdout, &arg->string_data, arg->string_length}
					static_call stream, write_char, {slave->slave_stdout, char_lf}
					assign {index + 1}, {index}
					static_call sys_task, yield
				loop_end
			endif

			;clean up
			static_call slave, deref, {slave}
		endif
		pop_scope
		return

	def_function_end
