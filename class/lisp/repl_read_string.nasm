%include 'inc/func.inc'
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

		ptr this, stream, string, char_str, tmp_str
		ulong char

		push_scope
		retire {r0, r1, r2}, {this, stream, char}

		static_call stream, read_char, {stream}, {char}
		static_call string, create_from_cstr, {""}, {string}
		loop_while {char != -1 && char != char_double_quote}
			assign {string}, {tmp_str}
			static_call string, create_from_cstr, {&char}, {char_str}
			static_call string, add, {string, char_str}, {string}
			static_call ref, deref, {char_str}
			static_call ref, deref, {tmp_str}
			static_call stream, read_char, {stream}, {char}
		loop_end
		static_call stream, read_char, {stream}, {char}

		eval {this, string, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
