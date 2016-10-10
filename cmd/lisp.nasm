%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_function cmd/lisp

		ptr slave, lisp, args, arg, stream, file
		ulong argc, index

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;create lisp class
			static_call lisp, create, {slave->slave_stdin, slave->slave_stdout, slave->slave_stderr}, {lisp}

			;run any lisp.lisp
			static_call string, create_from_file, {"cmd/lisp.lisp"}, {file}
			if {file}
				;REPL from file stream
				static_call stream_str, create, {file}, {stream}
				static_call lisp, repl, {lisp, stream}
				static_call stream, deref, {stream}
			endif

			;run any files given as args
			static_call slave, get_args, {slave}, {args}
			slot_call vector, get_length, {args}, {argc}
			assign {1}, {index}
			loop_while {index != argc}
				static_call vector, get_element, {args, index}, {arg}
				static_call string, create_from_file, {&arg->string_data}, {file}
				if {file}
					;REPL from file stream
					static_call stream_str, create, {file}, {stream}
					static_call lisp, repl, {lisp, stream}
					static_call stream, deref, {stream}
				endif
				assign {index + 1}, {index}
			loop_end

			;REPL from stdin
			static_call lisp, repl, {lisp, lisp->lisp_stdin}

			;clean up
			static_call lisp, deref, {lisp}
			static_call slave, deref, {slave}
		endif

		pop_scope
		return

	def_function_end
