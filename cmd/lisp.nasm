%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_slave.inc'

%undef debug_mode

	def_function cmd/lisp

		def_structure globals
			ptr globals_symbols
			ptr globals_enviroment
		def_structure_end

		const char_lf, 10

		ptr slave, ast, value
		ulong char
		struct globals, globals

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {slave}
		if {slave}
			;interned symbols set and enviroment
			static_call unordered_set, create, {@class/string/compare, 31}, {globals.globals_symbols}
			static_call unordered_map, create, {@class/string/compare, 31}, {globals.globals_enviroment}

			;REPL
			static_call stream, read_char, {slave->slave_stdin}, {char}
			debug_long "repl: ", r1
			loop_start
				local_call repl_read, {&globals, slave->slave_stdin, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
				breakif {!ast}
				local_call repl_eval, {&globals, ast}, {r0, r1}, {r0}, {value}
				static_call ref, deref, {ast}
				breakif {!value}
				local_call repl_print, {slave->slave_stdout, value}, {r0, r1}
				static_call ref, deref, {value}
				static_call stream, write_char, {slave->slave_stdout, char_lf}
				method_call stream, write_flush, {slave->slave_stdout}
			loop_end

			;clean up
			static_call unordered_set, deref, {globals.globals_symbols}
			static_call unordered_map, deref, {globals.globals_enviroment}
			static_call slave, deref, {slave}
		endif
		pop_scope
		return

	repl_read:
		;inputs
		;r0 = globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = 0, else ast
		;r1 = next char

		const char_space, ' '
		const char_lb, '('
		const char_rb, ')'

		ptr globals, stream, ast
		ulong char

		debug_long "repl_read: input ", r2

		push_scope
		retire {r0, r1, r2}, {globals, stream, char}

		;skip white space
		loop_while {char <= char_space && char != -1}
			static_call stream, read_char, {stream}, {char}
			debug_long "repl_read: white space ", r1
		loop_end

		;what are we reading ?
		assign {0}, {ast}
		if {char != -1}
			if {char == char_lb}
				local_call repl_read_list, {globals, stream}, {r0, r1}, {r0}, {ast}
				static_call stream, read_char, {stream}, {char}
				debug_long "repl_read: after list ", r1
			elseif {char == char_rb}
				static_call string, create_from_cstr, {"Error: unexpected )"}, {ast}
				static_call stream, read_char, {stream}, {char}
				debug_long "repl_read: after error", r1
			else
				local_call repl_read_symbol, {globals, stream, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
			endif
		endif

		eval {ast, char}, {r0, r1}
		pop_scope
		return

	repl_read_list:
		;inputs
		;r0 = globals
		;r1 = stream
		;outputs
		;r0 = list

		const char_space, ' '
		const char_rb, ')'

		ptr globals, stream, list, ast
		ulong char

		push_scope
		retire {r0, r1}, {globals, stream}

		;skip white space
		loop_start
			static_call stream, read_char, {stream}, {char}
			debug_long "repl_read_list: white space 1", r1
		loop_until {char > char_space || char == -1}

		static_call vector, create, {}, {list}
		loop_while {char != -1 && char != char_rb}
			local_call repl_read, {globals, stream, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
			breakif {!ast}
			static_call vector, push_back, {list, ast}
			debug_long "repl_read_list: pushed ", r1

			;skip white space
			loop_while {char <= char_space && char != -1}
				static_call stream, read_char, {stream}, {char}
				debug_long "repl_read_list: white space 2", r1
			loop_end
		loop_end

		eval {list}, {r0}
		pop_scope
		return

	repl_read_symbol:
		;inputs
		;r0 = globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = symbol
		;r1 = next char

		const char_space, ' '
		const char_lb, '('
		const char_rb, ')'

		ptr globals, stream, symbol, char_str, tmp_str
		pptr iter
		ulong char

		debug_long "repl_read_symbol: in ", r2

		push_scope
		retire {r0, r1, r2}, {globals, stream, char}

		static_call string, create_from_cstr, {""}, {symbol}
		loop_while {char > char_space && char != char_lb && char != char_rb}
			assign {symbol}, {tmp_str}
			static_call string, create_from_cstr, {&char}, {char_str}
			static_call string, add, {symbol, char_str}, {symbol}
			static_call ref, deref, {char_str}
			static_call ref, deref, {tmp_str}
			static_call stream, read_char, {stream}, {char}
			debug_long "repl_read_symbol: next char", r1
		loop_end

		;intern the symbol
		static_call unordered_set, insert, {globals->globals_symbols, symbol}, {iter, _}
		static_call string, deref, {symbol}
		assign {*iter}, {symbol}
		static_call string, ref, {symbol}

		eval {symbol, char}, {r0, r1}
		pop_scope
		return

	repl_eval:
		;inputs
		;r0 = globals
		;r1 = ast
		;outputs
		;r0 = 0, else value

		ptr globals, ast, value

		push_scope
		retire {r0, r1}, {globals, ast}

		assign {ast}, {value}
		static_call ref, ref, {value}

		eval {value}, {r0}
		pop_scope
		return

	repl_print:
		;inputs
		;r0 = stream
		;r1 = value

		const char_space, 32
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'

		ptr stream, value

		push_scope
		retire {r0, r1}, {stream, value}

		if {value->obj_vtable == @class/class_string}
			;symbol
			static_call stream, write, {stream, &value->string_data, value->string_length}
		elseif {value->obj_vtable == @class/class_vector}
			;list
			static_call stream, write_char, {stream, char_lb}
			static_call stream, write_char, {stream, char_space}
			static_call vector, for_each, {value, $repl_print_callback, stream}, {_}
			static_call stream, write_char, {stream, char_rb}
		endif
		static_call stream, write_char, {stream, char_space}

		pop_scope
		return

	repl_print_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr stream

		push_scope
		retire {r0, r1}, {iter, stream}

		local_call repl_print, {stream, *iter}, {r0, r1}

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
