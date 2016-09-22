%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_slave.inc'
%include 'inc/string.inc'

;%undef debug_mode
;%xdefine debug_lines

	def_function cmd/lisp

		;push the contants scope
		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_al, '<'
		const char_ar, '>'
		push_scope

		def_structure globals
			ptr globals_symbols
			ptr globals_enviroment
			ptr globals_slave

			;same order as builtin table
			ptr globals_sym_parent
			ptr globals_sym_nil
			ptr globals_sym_t
			ptr globals_sym_def
			ptr globals_sym_quote
		def_structure_end

		struct globals, globals
		ptr ast, value, symbol
		pubyte next
		pptr built_in
		ulong char, length

		;init app vars
		push_scope

		;initialize pipe details and command args, abort on error
		static_call slave, create, {}, {globals.globals_slave}
		if {globals.globals_slave}
			;interned symbols set and enviroment
			static_call unordered_set, create, {@class/string/compare, 31}, {globals.globals_symbols}
			static_call unordered_map, create, {@class/string/compare, 31}, {globals.globals_enviroment}

			;intern standard built in symbols
			assign {$built_ins, &globals.globals_sym_parent}, {next, built_in}
			loop_while {*next}
				static_call string, create_from_cstr, {next}, {symbol}
				local_call sym_intern, {&globals, symbol}, {r0, r1}, {r0}, {*built_in}
				static_call string, deref, {symbol}
				static_call sys_string, length, {next}, {length}
				assign {next + length + 1}, {next}
				assign {built_in + ptr_size}, {built_in}
			loop_end

			;standard self evaulating symbols
			local_call env_set, {&globals, globals.globals_sym_nil, globals.globals_sym_nil}, {r0, r1, r2}
			local_call env_set, {&globals, globals.globals_sym_t, globals.globals_sym_t}, {r0, r1, r2}
			local_call env_set, {&globals, globals.globals_sym_def, globals.globals_sym_def}, {r0, r1, r2}
			local_call env_set, {&globals, globals.globals_sym_quote, globals.globals_sym_quote}, {r0, r1, r2}

			;REPL
			static_call stream, read_char, {globals.globals_slave->slave_stdin}, {char}
			loop_start
				method_call stream, write_flush, {globals.globals_slave->slave_stdout}
				static_call sys_task, yield
				method_call stream, write_flush, {globals.globals_slave->slave_stderr}

				local_call repl_read, {&globals, globals.globals_slave->slave_stdin, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
				breakif {char == -1}
				continueif {!ast}

				local_call repl_print, {&globals, ast}, {r0, r1}
				static_call stream, write_char, {globals.globals_slave->slave_stdout, char_lf}

				local_call repl_eval, {&globals, ast}, {r0, r1}, {r0}, {value}
				static_call ref, deref, {ast}
				continueif {!value}
				local_call repl_print, {&globals, value}, {r0, r1}
				static_call ref, deref, {value}
				static_call stream, write_char, {globals.globals_slave->slave_stdout, char_lf}
			loop_end

			;clean up
			static_call unordered_set, deref, {globals.globals_symbols}
			static_call unordered_map, deref, {globals.globals_enviroment}
			static_call slave, deref, {globals.globals_slave}
		endif
		pop_scope
		return

	error:
		;inputs
		;r0 = globals
		;r1 = error string

		ptr globals, stderr

		push_scope
		retire {r0}, {globals}

		assign {globals->globals_slave->slave_stderr}, {stderr}
		static_call stream, write_cstr, {stderr, r1}
		static_call stream, write_char, {stderr, char_lf}

		pop_scope
		return

;;;;;;;;;
; symbols
;;;;;;;;;

	sym_intern:
		;inputs
		;r0 = globals
		;r1 = symbol
		;outputs
		;r0 = interned symbol

		ptr globals, symbol
		pptr iter

		push_scope
		retire {r0, r1}, {globals, symbol}

		static_call unordered_set, insert, {globals->globals_symbols, symbol}, {iter, _}
		static_call string, deref, {symbol}
		assign {*iter}, {symbol}
		static_call string, ref, {symbol}

		eval {symbol}, {r0}
		pop_scope
		return

;;;;;;;;;;;;
; enviroment
;;;;;;;;;;;;

	env_find:
		;inputs
		;r0 = globals
		;r1 = symbol
		;outputs
		;r0 = 0, else iterator
		;r1 = bucket vector

		ptr globals, symbol, bucket, env
		pptr iter

		push_scope
		retire {r0, r1}, {globals, symbol}

		assign {globals->globals_enviroment}, {env}
		loop_start
			static_call unordered_map, find, {env, symbol}, {iter, bucket}
			breakif {iter}
			static_call unordered_map, find, {env, globals->globals_sym_parent}, {iter, bucket}
			breakif {!iter}
			assign {*iter}, {env}
		loop_end

		eval {iter, bucket}, {r0, r1}
		pop_scope
		return

	env_set:
		;inputs
		;r0 = globals
		;r1 = symbol
		;r2 = value

		ptr globals, symbol, value
		pptr iter

		push_scope
		retire {r0, r1, r2}, {globals, symbol, value}

		local_call env_find, {globals, symbol}, {r0, r1}, {r0, r1}, {iter, _}
		if {iter}
			;change existing value
			static_call ref, ref, {value}
			static_call pair, set_second, {*iter, value}
		else
			;new variable
			static_call unordered_map, insert, {globals->globals_enviroment, symbol, value}, {_, _}
		endif

		pop_scope
		return

	env_get:
		;inputs
		;r0 = globals
		;r1 = symbol
		;outputs
		;r0 = 0, else value

		ptr globals, symbol, value
		pptr iter

		push_scope
		retire {r0, r1}, {globals, symbol}

		local_call env_find, {globals, symbol}, {r0, r1}, {r0, r1}, {iter, _}
		if {iter}
			;found
			static_call pair, ref_second, {*iter}, {value}
		else
			;not found
			assign {0}, {value}
		endif

		eval {value}, {r0}
		pop_scope
		return

;;;;;;
; read
;;;;;;

	repl_read:
		;inputs
		;r0 = globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = 0, else ast
		;r1 = next char

		ptr globals, stream, ast
		ulong char

		push_scope
		retire {r0, r1, r2}, {globals, stream, char}

		;skip white space
		loop_while {char <= char_space && char != -1}
			static_call stream, read_char, {stream}, {char}
		loop_end

		;what are we reading ?
		assign {0}, {ast}
		if {char != -1}
			if {char == char_lb}
				local_call repl_read_list, {globals, stream}, {r0, r1}, {r0}, {ast}
				static_call stream, read_char, {stream}, {char}
			elseif {char == char_rb}
				local_call error, {globals, "Error: unexpected )"}, {r0, r1}
				static_call stream, read_char, {stream}, {char}
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

		ptr globals, stream, list, ast
		ulong char

		push_scope
		retire {r0, r1}, {globals, stream}

		;skip white space
		loop_start
			static_call stream, read_char, {stream}, {char}
		loop_until {char > char_space || char == -1}

		static_call vector, create, {}, {list}
		loop_while {char != -1 && char != char_rb}
			local_call repl_read, {globals, stream, char}, {r0, r1, r2}, {r0, r1}, {ast, char}
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
		;r0 = globals
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = symbol
		;r1 = next char

		ptr globals, stream, symbol, char_str, tmp_str
		ulong char

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
		loop_end

		;intern the symbol
		local_call sym_intern, {globals, symbol}, {r0, r1}, {r0}, {symbol}

		eval {symbol, char}, {r0, r1}
		pop_scope
		return

;;;;;;
; eval
;;;;;;

	repl_eval:
		;inputs
		;r0 = globals
		;r1 = ast
		;outputs
		;r0 = 0, else value

		ptr globals, ast, value, func
		ulong length

		push_scope
		retire {r0, r1}, {globals, ast}

		;evaluate based on type
		if {ast->obj_vtable == @class/class_string}
			;symbol evals to its value
			local_call env_get, {globals, ast}, {r0, r1}, {r0}, {value}
			breakif {value}
			local_call error, {globals, "Error: variable not defined"}, {r0, r1}
		elseif {ast->obj_vtable == @class/class_vector}
			;list
			static_call vector, get_length, {ast}, {length}
			if {!length}
				;null list evals to nil
				assign {globals->globals_sym_nil}, {value}
				static_call ref, ref, {value}
			elseif {length == 1}
				;one entry evals to that entry
				static_call vector, get_element, {ast, 0}, {value}
				local_call repl_eval, {globals, value}, {r0, r1}, {r0}, {value}
			else
				;more than one entry calls first as function on remaining
				static_call vector, get_element, {ast, 0}, {func}
				local_call repl_eval, {globals, func}, {r0, r1}, {r0}, {func}
				assign {0}, {value}
				if (func)
					if {func->obj_vtable == @class/class_string}
						;built in ?
						if {func == globals->globals_sym_quote}
							local_call repl_eval_quote, {globals, ast}, {r0, r1}, {r0}, {value}
						elseif {func == globals->globals_sym_def}
							local_call repl_eval_def, {globals, ast}, {r0, r1}, {r0}, {value}
						else
							local_call error, {globals, "Error: not implamented"}, {r0, r1}
						endif
					else
						local_call error, {globals, "Error: not implamented"}, {r0, r1}
					endif
					static_call ref, deref, {func}
				endif
			endif
		endif

		eval {value}, {r0}
		pop_scope
		return

	repl_eval_quote:
		;inputs
		;r0 = globals
		;r1 = list
		;outputs
		;r0 = 0, else value

		ptr globals, list, symbol, value
		ulong length

		push_scope
		retire {r0, r1}, {globals, list}

		static_call vector, get_length, {list}, {length}
		if {length != 2}
			local_call error, {globals, "Error: (quote arg) wrong numbers of args"}, {r0, r1}
			assign {0}, {value}
		else
			static_call vector, ref_element, {list, 1}, {value}
		endif

		eval {value}, {r0}
		pop_scope
		return

	repl_eval_list:
		;inputs
		;r0 = globals
		;r1 = list
		;outputs
		;r0 = 0 if error

		ptr globals, list

		push_scope
		retire {r0, r1}, {globals, list}

		if {list->obj_vtable != @class/class_vector}
			local_call error, {globals, "Error: not a list"}, {r0, r1}
			assign {0}, {list}
		else
			static_call vector, for_each, {list, $repl_eval_list_callback, globals}, {list}
			assign {!list}, {list}
		endif

		eval {list}, {r0}
		pop_scope
		return

	repl_eval_list_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr globals

		push_scope
		retire {r0, r1}, {iter, globals}

		local_call repl_eval, {globals, *iter}, {r0, r1}, {r0}, {globals}
		if {globals}
			static_call ref, deref, {*iter}
			assign {globals}, {*iter}
		endif

		eval {globals}, {r1}
		pop_scope
		return

	repl_eval_def:
		;inputs
		;r0 = globals
		;r1 = list
		;outputs
		;r0 = 0, else value

		ptr globals, list, value, symbol, vars, vals
		ulong length, ok

		push_scope
		retire {r0, r1}, {globals, list}

		assign {0}, {value}
		static_call vector, get_length, {list}, {length}
		if {length != 3}
			local_call error, {globals, "Error: (def (vars) (vals)) wrong numbers of args"}, {r0, r1}
		else
			;eval args
			static_call vector, slice, {list, 1, length}, {list}
			local_call repl_eval_list, {globals, list}, {r0, r1}, {r0}, {ok}
			if {ok}
				static_call vector, get_element, {list, 0}, {vars}
				if {vars->obj_vtable != @class/class_vector}
					local_call error, {globals, "Error: (def (vars) (vals)): (vars) not a list"}, {r0, r1}
				else
					static_call vector, get_element, {list, 1}, {vals}
					if {vals->obj_vtable != @class/class_vector}
						local_call error, {globals, "Error: (def (vars) (vals)): (vals) not a list"}, {r0, r1}
					else
						static_call vector, get_length, {vars}, {length}
						static_call vector, get_length, {vals}, {ok}
						if {length != ok}
							local_call error, {globals, "Error: (def (vars) (vals)): non matching lengths"}, {r0, r1}
						else
							assign {0}, {ok}
							loop_while {ok != length}
								static_call vector, get_element, {vars, ok}, {symbol}
								static_call vector, get_element, {vals, ok}, {value}
								local_call env_set, {globals, symbol, value}, {r0, r1, r2}
								assign {ok + 1}, {ok}
							loop_end
						endif
					endif
				endif
			endif
			static_call vector, deref, {list}
		endif

		eval {value}, {r0}
		pop_scope
		return

;;;;;;;
; print
;;;;;;;

	repl_print:
		;inputs
		;r0 = globals
		;r1 = value

		ptr globals, value, stream

		push_scope
		retire {r0, r1}, {globals, value}

		assign {globals->globals_slave->slave_stdout}, {stream}
		if {value->obj_vtable == @class/class_string}
			;symbol
			static_call stream, write, {stream, &value->string_data, value->string_length}
		elseif {value->obj_vtable == @class/class_vector}
			;list
			static_call stream, write_char, {stream, char_lb}
			static_call stream, write_char, {stream, char_space}
			static_call vector, for_each, {value, $repl_print_callback, globals}, {_}
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
		ptr globals

		push_scope
		retire {r0, r1}, {iter, globals}

		local_call repl_print, {globals, *iter}, {r0, r1}

		eval {1}, {r1}
		pop_scope
		return

;;;;;;;;;;;;;;;;;;
; built in symbols
;;;;;;;;;;;;;;;;;;

	built_ins:
		db "_parent_", 0
		db "nil", 0
		db "t", 0
		db "def", 0
		db "quote", 0
		db 0

		;pop the contants scope
		pop_scope

	def_function_end
