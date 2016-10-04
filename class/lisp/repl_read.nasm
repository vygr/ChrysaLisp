%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_read
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = lisp object
		;r1 = 0, else ast
		;r2 = next char

		const char_space, ' '
		const char_lb, '('
		const char_rb, ')'
		const char_0, '0'
		const char_9, '9'
		const char_minus, '-'
		const char_single_quote, "'"
		const char_double_quote, '"'

		ptr this, stream, ast, elem
		ulong char

		push_scope
		retire {r0, r1, r2}, {this, stream, char}

		;skip white space
		loop_while {char <= char_space && char != -1}
			static_call stream, read_char, {stream}, {char}
		loop_end

		;what are we reading ?
		assign {0}, {ast}
		if {char != -1}
			if {char == char_lb}
				static_call lisp, repl_read_list, {this, stream}, {ast}
				static_call stream, read_char, {stream}, {char}
			elseif {char == char_rb}
				static_call lisp, error, {this, "unexpected )", ast}
				static_call stream, read_char, {stream}, {char}
			elseif {char == char_minus || (char >= char_0 && char <= char_9)}
				static_call lisp, repl_read_num, {this, stream, char}, {ast, char}
			elseif {char == char_single_quote}
				static_call vector, create, {}, {ast}
				assign {this->lisp_sym_quote}, {elem}
				static_call ref, ref, {elem}
				static_call vector, push_back, {ast, elem}
				static_call stream, read_char, {stream}, {char}
				static_call lisp, repl_read, {this, stream, char}, {elem, char}
				static_call vector, push_back, {ast, elem}
			elseif {char == char_double_quote}
				static_call lisp, repl_read_string, {this, stream, char}, {ast, char}
			else
				static_call lisp, repl_read_sym, {this, stream, char}, {ast, char}
			endif
		endif

		eval {this, ast, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
