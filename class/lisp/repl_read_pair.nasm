%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_pair.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_read_pair
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = lisp object
		;r1 = list
		;r2 = next char

		const char_space, ' '
		const char_rab, '>'

		ptr this, stream, pair, first, second
		ulong char

		push_scope
		retire {r0, r1, r2}, {this, stream, char}

		;skip "<"
		static_call stream, read_char, {stream}, {char}

		assign {0}, {pair}
		static_call lisp, repl_read, {this, stream, char}, {first, char}
		gotoifnot {first}, error
		static_call lisp, repl_read, {this, stream, char}, {second, char}
		gotoifnot {second}, error1

		;skip white space
		loop_while {char <= char_space && char != -1}
			static_call stream, read_char, {stream}, {char}
		loop_end

		if {char == char_rab}
			static_call stream, read_char, {stream}, {char}
			static_call pair, create, {first, second}, {pair}
		else
			static_call ref, deref, {second}
		error1:
			static_call ref, deref, {first}
		error:
		endif

		eval {this, pair, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
