%include 'inc/func.inc'
%include 'class/class_symbol.inc'
%include 'class/class_stream.inc'
%include 'class/class_boxed_long.inc'
%include 'inc/string.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_read_num
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = lisp object
		;r1 = number
		;r2 = next char

		const char_0, '0'
		const char_9, '9'
		const char_minus, '-'

		ptr this, stream, num, symbol, char_str, tmp_str
		ulong char, sign
		long val

		push_scope
		retire {r0, r1, r2, r2}, {this, stream, char, sign}

		if {char == char_minus}
			static_call stream, read_char, {stream}, {char}
		endif

		static_call symbol, create_from_cstr, {""}, {symbol}
		loop_while {char >= char_0 && char <= char_9}
			assign {symbol}, {tmp_str}
			static_call symbol, create_from_cstr, {&char}, {char_str}
			static_call symbol, add, {symbol, char_str}, {symbol}
			static_call ref, deref, {char_str}
			static_call ref, deref, {tmp_str}
			static_call stream, read_char, {stream}, {char}
		loop_end

		;create the number
		static_call boxed_long, create, {}, {num}
		static_call sys_string, to_long, {&symbol->string_data, 10}, {val}
		if {sign == char_minus}
			assign {-val}, {val}
		endif
		static_call boxed_long, set_value, {num, val}
		static_call ref, deref, {symbol}

		eval {this, num, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
