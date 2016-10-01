%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_structure built_in
		ushort built_in_field
		ushort built_in_symbol
		ushort built_in_function
		offset built_in_hop ;built_in_size would be aligned to long !
	def_structure_end

	%macro built_in 2-3
		;%1 = field
		;%2 = symbol
		;%3 = function
		align 2, db 0
		dw %1
		fn_add_string %2
		dw _ref_%[_string_num_]_string - $
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
		pptr field_ptr, path_ptr
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
			static_call unordered_map, create, {@_function_, 31}, {this->lisp_enviroment}

			;intern standard built in symbols
			;fixup built in functions
			assign {$built_ins}, {table}
			loop_while {table->built_in_field}
				assign {&table->built_in_symbol + table->built_in_symbol}, {path_ptr}
				static_call lisp, sym_intern_cstr, {this, path_ptr}, {symbol}
				assign {this + table->built_in_field}, {field_ptr}
				assign {symbol}, {*field_ptr}
				if {table->built_in_function}
					assign {&table->built_in_function + table->built_in_function}, {path_ptr}
					static_call lisp, built_in_func, {this, *field_ptr, *path_ptr}
				endif
				assign {table + built_in_hop}, {table}
			loop_end

			;standard self evaulating symbols
			static_call lisp, env_set, {this, this->lisp_sym_nil, this->lisp_sym_nil}
			static_call lisp, env_set, {this, this->lisp_sym_t, this->lisp_sym_t}
		endif

		eval {this, ok}, {r0, r1}
		pop_scope
		return

;;;;;;;;;;;
; built ins
;;;;;;;;;;;

	built_ins:
		built_in lisp_sym_parent, "_parent_"
		built_in lisp_sym_nil, "nil"
		built_in lisp_sym_t, "t"
		built_in lisp_sym_lambda, "lambda", func_lambda
		built_in lisp_sym_def, "def", func_def
		built_in lisp_sym_quote, "quote", func_quote
		built_in lisp_sym_list, "list", func_list
		built_in lisp_sym_add, "add", func_add
		built_in lisp_sym_sub, "sub", func_sub
		built_in lisp_sym_mul, "mul", func_mul
		built_in lisp_sym_div, "div", func_div
		built_in lisp_sym_mod, "mod", func_mod
		built_in lisp_sym_eq, "eq", func_eq
		built_in lisp_sym_cond, "cond", func_cond
		built_in lisp_sym_progn, "progn", func_progn
		built_in lisp_sym_not, "not", func_not
		built_in lisp_sym_and, "and", func_and
		built_in lisp_sym_or, "or", func_or
		built_in lisp_sym_when, "when", func_when
		built_in lisp_sym_unless, "unless", func_unless
		built_in lisp_sym_if, "if", func_if
		built_in lisp_sym_map, "map", func_map
		built_in lisp_sym_some, "some", func_some
		built_in lisp_sym_every, "every", func_every
		built_in lisp_sym_notany, "notany", func_notany
		built_in lisp_sym_notevery, "notevery", func_notevery
		built_in lisp_sym_length, "length", func_length
		built_in lisp_sym_while, "while", func_while
		built_in lisp_sym_print, "print", func_print
		built_in lisp_sym_prin, "prin", func_prin
		built_in lisp_sym_env, "env", func_env
		dw 0

	def_function_end
