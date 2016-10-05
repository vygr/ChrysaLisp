%include 'inc/func.inc'
%include 'inc/load.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_read_string
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = lisp object
		;r1 = string
		;r2 = next char

		const char_double_quote, '"'

		ptr this, stream, string
		pubyte relloc, buffer
		ulong char

		push_scope
		retire {r0, r1, r2}, {this, stream, char}

		slot_function sys_load, statics
		assign {@_function_.ld_statics_reloc_buffer}, {relloc}
		assign {relloc}, {buffer}

		static_call stream, read_char, {stream}, {char}
		loop_while {char != -1 && char != char_double_quote}
			assign {char}, {*buffer}
			assign {buffer + 1}, {buffer}
			static_call stream, read_char, {stream}, {char}
		loop_end
		static_call stream, read_char, {stream}, {char}

		static_call string, create_from_buffer, {relloc, buffer - relloc}, {string}

		eval {this, string, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
