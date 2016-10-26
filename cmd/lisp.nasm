%include 'inc/func.inc'
%include 'class/class_slave.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_func cmd/lisp

		ptr slave, lisp, args, arg, stream, file
		ulong argc, index

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		func_call slave, create, {}, {slave}
		if {slave}
			;create lisp class
			func_call lisp, create, {slave->slave_stdin, slave->slave_stdout, slave->slave_stderr}, {lisp}

			;run any lisp.lisp
			func_call string, create_from_file, {"cmd/lisp.lisp"}, {file}
			if {file}
				;REPL from file stream
				func_call stream_str, create, {file}, {stream}
				func_call lisp, repl, {lisp, stream}
				func_call stream, deref, {stream}
			endif

			;run any files given as args
			func_call slave, get_args, {slave}, {args}
			devirt_call vector, get_length, {args}, {argc}
			assign {1}, {index}
			loop_while {index != argc}
				func_call vector, get_element, {args, index}, {arg}
				func_call string, create_from_file, {&arg->string_data}, {file}
				if {file}
					;REPL from file stream
					func_call stream_str, create, {file}, {stream}
					func_call lisp, repl, {lisp, stream}
					func_call stream, deref, {stream}
				endif
				assign {index + 1}, {index}
			loop_end

			;REPL from stdin
			func_call lisp, repl, {lisp, lisp->lisp_stdin}

			;clean up
			func_call lisp, deref, {lisp}
			func_call slave, deref, {slave}
		endif

		pop_scope
		return

	def_func_end
