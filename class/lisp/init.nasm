%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_structure built_in
		ushort built_in_symbol
		ushort built_in_field
		ushort built_in_function
		offset built_in_hop
		;built_in_size would be aligned to long !
	def_structure_end

	%macro built_in 2-3
		;%1 = symbol
		;%2 = field
		;%3 = function
		fn_add_string %1
		dw _ref_%[_string_num_]_string - $
		dw %2
		%if %0 = 2
			dw 0
		%else
			slot_function lisp, %3
			fn_find_link _function_
			dw _ref_%[_link_num_]_link - $
		%endif
	%endmacro

	def_function class/lisp/init
		;inputs
		;r0 = object
		;r1 = vtable pointer
		;r2 = stdin stream
		;r3 = stdout stream
		;r5 = stderr stream
		;outputs
		;r0 = object
		;r1 = 0 if error, else ok
		;trashes
		;all

		ptr this, vtable, stdin, stdout, stderr, symbol, table
		pptr path_ptr
		ulong ok

		push_scope
		retire {r0, r1, r2, r3, r5}, {this, vtable, stdin, stdout, stderr}

		;init parent
		super_call lisp, init, {this, vtable}, {ok}
		if {ok}
			;init myself
			assign {stdin}, {this->lisp_stdin}
			assign {stdout}, {this->lisp_stdout}
			assign {stderr}, {this->lisp_stderr}

			;interned symbols set and enviroment
			slot_function string, compare
			static_call unordered_set, create, {@_function_, 31}, {this->lisp_symbols}
			static_call unordered_map, create, {$match_obj, 31}, {this->lisp_enviroment}

			;intern standard built in symbols
			;fixup built in functions
			assign {$built_ins}, {table}
			loop_while {table->built_in_symbol}
				assign {&table->built_in_symbol + table->built_in_symbol}, {path_ptr}
				static_call lisp, sym_intern_cstr, {this, path_ptr}, {symbol}
				if {table->built_in_field}
					assign {this + table->built_in_field}, {path_ptr}
					assign {symbol}, {*path_ptr}
				endif
				if {table->built_in_function}
					assign {&table->built_in_function + table->built_in_function}, {path_ptr}
					static_call lisp, built_in_func, {this, symbol, *path_ptr}
				endif
				assign {table + built_in_hop}, {table}
			loop_end

			;standard self evaulating symbols
			static_call lisp, env_def, {this, this->lisp_sym_nil, this->lisp_sym_nil}
			static_call lisp, env_def, {this, this->lisp_sym_t, this->lisp_sym_t}
		endif

		eval {this, ok}, {r0, r1}
		pop_scope
		return

	match_obj:
		;inputs
		;r0 = object 1
		;r1 = object 2
		;outputs
		;r1 = 0 if match

		if r0, ==, r1
			vp_xor r1, r1
		endif
		vp_ret

;;;;;;;;;;;
; built ins
;;;;;;;;;;;

		align 2, db 0
	built_ins:
		built_in "_parent_", lisp_sym_parent
		built_in "nil", lisp_sym_nil
		built_in "t", lisp_sym_t
		built_in "lambda", lisp_sym_lambda, func_lambda
		built_in "quote", lisp_sym_quote, func_quote
		built_in "def", 0, func_def
		built_in "set", 0, func_set
		built_in "setl", 0, func_setl
		built_in "list", 0, func_list
		built_in "add", 0, func_add
		built_in "sub", 0, func_sub
		built_in "mul", 0, func_mul
		built_in "div", 0, func_div
		built_in "mod", 0, func_mod
		built_in "eq", 0, func_eq
		built_in "cond", 0, func_cond
		built_in "progn", 0, func_progn
		built_in "not", 0, func_not
		built_in "and", 0, func_and
		built_in "or", 0, func_or
		built_in "when", 0, func_when
		built_in "unless", 0, func_unless
		built_in "if", 0, func_if
		built_in "map", 0, func_map
		built_in "some", 0, func_some
		built_in "every", 0, func_every
		built_in "notany", 0, func_notany
		built_in "notevery", 0, func_notevery
		built_in "length", 0, func_length
		built_in "while", 0, func_while
		built_in "until", 0, func_until
		built_in "print", 0, func_print
		built_in "prin", 0, func_prin
		built_in "env", 0, func_env
		built_in "str", 0, func_str
		dw 0

	def_function_end
