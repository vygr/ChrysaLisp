%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/repl_read_list
		;inputs
		;r0 = lisp object
		;r1 = stream
		;outputs
		;r0 = lisp object
		;r1 = list

		const char_space, ' '
		const char_rb, ')'

		ptr this, stream, list, ast
		ulong char

		push_scope
		retire {r0, r1}, {this, stream}

		;skip white space
		loop_start
			static_call stream, read_char, {stream}, {char}
		loop_until {char > char_space || char == -1}

		static_call vector, create, {}, {list}
		loop_while {char != -1 && char != char_rb}
			static_call lisp, repl_read, {this, stream, char}, {ast, char}
			breakif {!ast}
			static_call vector, push_back, {list, ast}

			;skip white space
			loop_while {char <= char_space && char != -1}
				static_call stream, read_char, {stream}, {char}
			loop_end
		loop_end

		eval {this, list}, {r0, r1}
		pop_scope
		return

	def_function_end
