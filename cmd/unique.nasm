%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_slave.inc'
%include 'class/class_unordered_set.inc'

	def_function cmd/unique

		buffer_size equ 120

		ptr slave, arg, args, stream, set
		ulong argc, index, length
		struct buffer, buffer

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;create string set
			static_call unordered_set, create, {@class/string/compare, 31}, {set}

			;arg 1 is command name
			static_call slave, get_args, {slave}, {args}
			slot_call vector, get_length, {args}, {argc}
			if {argc != 1}
				;from command line
				assign {1}, {index}
				loop_while {index != argc}
					static_call vector, get_element, {args, index}, {arg}
					static_call unordered_set, insert, {set, arg}, {_, _}
					assign {index + 1}, {index}
				loop_end
			else
				;from stdin
				loop_start
					static_call stream, read_line, {slave->slave_stdin, &buffer, buffer_size}, {length}
					breakif {length == -1}
					static_call string, create_from_buffer, {&buffer, length}, {arg}
					static_call unordered_set, insert, {set, arg}, {_, _}
					static_call string, deref, {arg}
				loop_end
			endif

			;output string set
			static_call unordered_set, for_each, {set, $callback, slave}, {_, _}
			static_call unordered_set, deref, {set}

			;clean up
			static_call slave, deref, {slave}
		endif
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		const char_lf, 10
		pptr iter
		ptr slave

		push_scope
		retire {r0, r1}, {iter, slave}

		static_call stream, write, {slave->slave_stdout, &(*iter)->string_data, (*iter)->string_length}
		static_call stream, write_char, {slave->slave_stdout, char_lf}

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
