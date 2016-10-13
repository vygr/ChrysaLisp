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
		const char_lrb, '('
		const char_rrb, ')'
		const char_lab, '<'
		const char_rab, '>'
		const char_0, '0'
		const char_9, '9'
		const char_minus, '-'
		const char_quote, "'"
		const char_double_quote, '"'
		const char_quasi_quote, '`'
		const char_unquote, ','
		const char_splicing, '~'

		ptr this, stream, ast
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
			switch
			case {char == char_rrb}
				static_call lisp, error, {this, "unexpected )", ast}
				goto next_char
			case {char == char_rab}
				static_call lisp, error, {this, "unexpected >", ast}
			next_char:
				static_call stream, read_char, {stream}, {char}
				break
			case {char == char_lrb}
				static_call lisp, repl_read_list, {this, stream, char}, {ast, char}
				break
			case {char == char_lab}
				static_call lisp, repl_read_pair, {this, stream, char}, {ast, char}
				break
			case {char == char_minus || (char >= char_0 && char <= char_9)}
				static_call lisp, repl_read_num, {this, stream, char}, {ast, char}
				break
			case {char == char_double_quote}
				static_call lisp, repl_read_str, {this, stream, char}, {ast, char}
				break
			case {char == char_quote}
				static_call lisp, repl_read_quote, {this, stream, char}, {ast, char}
				break
			case {char == char_quasi_quote}
				static_call lisp, repl_read_qquote, {this, stream, char}, {ast, char}
				break
			case {char == char_unquote}
				static_call lisp, repl_read_unquote, {this, stream, char}, {ast, char}
				break
			case {char == char_splicing}
				static_call lisp, repl_read_splicing, {this, stream, char}, {ast, char}
				break
			default
				static_call lisp, repl_read_sym, {this, stream, char}, {ast, char}
			endswitch
		endif

		eval {this, ast, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
