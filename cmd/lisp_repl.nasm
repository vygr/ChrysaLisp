%include 'cmd/lisp/lisp.inc'

;%undef debug_mode
;%xdefine debug_lines

	def_function cmd/lisp_repl

		;push the contants scope
		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_al, '<'
		const char_ar, '>'
		push_scope

		struct lisp, lisp
		ptr ast, value, symbol
		pubyte next_byte
		pptr built_in
		ulong char, length

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {lisp.lisp_slave}
		if {lisp.lisp_slave}
			;interned symbols set and enviroment
			static_call unordered_set, create, {@class/string/compare, 31}, {lisp.lisp_symbols}
			static_call unordered_map, create, {@class/string/compare, 31}, {lisp.lisp_enviroment}

			;intern standard built in symbols
			assign {$built_in_symbols, &lisp.lisp_sym_parent}, {next_byte, built_in}
			loop_while {*next_byte}
				static_call lisp, sym_intern_cstr, {&lisp, next_byte}, {*built_in}
				static_call sys_string, length, {next_byte}, {length}
				assign {next_byte + length + 1}, {next_byte}
				assign {built_in + ptr_size}, {built_in}
			loop_end

			;standard self evaulating symbols
			static_call lisp, env_set, {&lisp, lisp.lisp_sym_nil, lisp.lisp_sym_nil}
			static_call lisp, env_set, {&lisp, lisp.lisp_sym_t, lisp.lisp_sym_t}

			;bind built in functions
			static_call lisp, built_in_func, {&lisp, lisp.lisp_sym_def, @cmd/lisp/func_def}
			static_call lisp, built_in_func, {&lisp, lisp.lisp_sym_quote, @cmd/lisp/func_quote}
			static_call lisp, built_in_func, {&lisp, lisp.lisp_sym_list, @cmd/lisp/func_list}

			;REPL
			static_call stream, read_char, {lisp.lisp_slave->slave_stdin}, {char}
			loop_start
				method_call stream, write_flush, {lisp.lisp_slave->slave_stdout}
				static_call sys_task, yield
				method_call stream, write_flush, {lisp.lisp_slave->slave_stderr}

				local_call repl_read, {&lisp, lisp.lisp_slave->slave_stdin, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
				breakif {char == -1}
				continueif {!ast}

				static_call stream, write_cstr, {lisp.lisp_slave->slave_stdout, "--AST--"}
				static_call stream, write_char, {lisp.lisp_slave->slave_stdout, char_lf}
				local_call repl_print, {&lisp, ast}, {r0, r1}
				static_call stream, write_char, {lisp.lisp_slave->slave_stdout, char_lf}

				static_call lisp, repl_eval, {&lisp, ast}, {value}
				static_call ref, deref, {ast}
				continueif {!value}

				static_call stream, write_cstr, {lisp.lisp_slave->slave_stdout, "--Value--"}
				static_call stream, write_char, {lisp.lisp_slave->slave_stdout, char_lf}
				local_call repl_print, {&lisp, value}, {r0, r1}
				static_call ref, deref, {value}
				static_call stream, write_char, {lisp.lisp_slave->slave_stdout, char_lf}
			loop_end

			;clean up
			static_call unordered_set, deref, {lisp.lisp_symbols}
			static_call unordered_map, deref, {lisp.lisp_enviroment}
			static_call slave, deref, {lisp.lisp_slave}
		endif
		pop_scope
		return

;;;;;;
; read
;;;;;;

	repl_read:
		;inputs
		;r0 = lisp globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = 0, else ast
		;r1 = next char

		ptr lisp, stream, ast
		ulong char

		push_scope
		retire {r0, r1, r2}, {lisp, stream, char}

		;skip white space
		loop_while {char <= char_space && char != -1}
			static_call stream, read_char, {stream}, {char}
		loop_end

		;what are we reading ?
		assign {0}, {ast}
		if {char != -1}
			if {char == char_lb}
				local_call repl_read_list, {lisp, stream}, {r0, r1}, {r0}, {ast}
				static_call stream, read_char, {stream}, {char}
			elseif {char == char_rb}
				static_call lisp, error, {lisp, "unexpected )"}
				static_call stream, read_char, {stream}, {char}
			else
				local_call repl_read_symbol, {lisp, stream, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
			endif
		endif

		eval {ast, char}, {r0, r1}
		pop_scope
		return

	repl_read_list:
		;inputs
		;r0 = lisp globals
		;r1 = stream
		;outputs
		;r0 = list

		ptr lisp, stream, list, ast
		ulong char

		push_scope
		retire {r0, r1}, {lisp, stream}

		;skip white space
		loop_start
			static_call stream, read_char, {stream}, {char}
		loop_until {char > char_space || char == -1}

		static_call vector, create, {}, {list}
		loop_while {char != -1 && char != char_rb}
			local_call repl_read, {lisp, stream, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
			breakif {!ast}
			static_call vector, push_back, {list, ast}

			;skip white space
			loop_while {char <= char_space && char != -1}
				static_call stream, read_char, {stream}, {char}
			loop_end
		loop_end

		eval {list}, {r0}
		pop_scope
		return

	repl_read_symbol:
		;inputs
		;r0 = lisp globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = symbol
		;r1 = next char

		ptr lisp, stream, symbol, char_str, tmp_str
		ulong char

		push_scope
		retire {r0, r1, r2}, {lisp, stream, char}

		static_call string, create_from_cstr, {""}, {symbol}
		loop_while {char > char_space && char != char_lb && char != char_rb}
			assign {symbol}, {tmp_str}
			static_call string, create_from_cstr, {&char}, {char_str}
			static_call string, add, {symbol, char_str}, {symbol}
			static_call ref, deref, {char_str}
			static_call ref, deref, {tmp_str}
			static_call stream, read_char, {stream}, {char}
		loop_end

		;intern the symbol
		static_call lisp, sym_intern, {lisp, symbol}, {symbol}

		eval {symbol, char}, {r0, r1}
		pop_scope
		return

;;;;;;;
; print
;;;;;;;

	repl_print:
		;inputs
		;r0 = lisp globals
		;r1 = value

		ptr lisp, value, stream

		push_scope
		retire {r0, r1}, {lisp, value}

		assign {lisp->lisp_slave->slave_stdout}, {stream}
		if {value->obj_vtable == @class/class_string}
			;symbol
			static_call stream, write, {stream, &value->string_data, value->string_length}
		elseif {value->obj_vtable == @class/class_boxed_ptr}
			;function pointer
			static_call stream, write_cstr, {stream, "<function>"}
		elseif {value->obj_vtable == @class/class_vector}
			;list
			static_call stream, write_char, {stream, char_lb}
			static_call stream, write_char, {stream, char_space}
			static_call vector, for_each, {value, $repl_print_callback, lisp}, {_}
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
		ptr lisp

		push_scope
		retire {r0, r1}, {iter, lisp}

		local_call repl_print, {lisp, *iter}, {r0, r1}

		eval {1}, {r1}
		pop_scope
		return

;;;;;;;;;;;;;;;;;;
; built in symbols
;;;;;;;;;;;;;;;;;;

	built_in_symbols:
		;all are symbols
		db "_parent_", 0
		db "nil", 0
		db "t", 0
		db "lambda", 0
		db "def", 0
		db "quote", 0
		db "list", 0
		db 0

		;pop the contants scope
		pop_scope

	def_function_end
