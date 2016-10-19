%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_symbol.inc'
%include 'class/class_lisp.inc'

	def_structure built_in
		ushort built_in_symbol
		ushort built_in_field
		ushort built_in_function
		ushort built_in_flags
		offset built_in_hop
		;built_in_size would be aligned to long !
	def_structure_end

	%macro built_in 2-4 "", 0
		;%1 = symbol
		;%2 = field
		;%3 = function
		;%4 = flags
		fn_add_string %1
		dw _ref_%[_string_num_]_string - $
		dw %2
		%ifidn %3, ""
			dw 0
		%else
			slot_function lisp, %3
			fn_find_link _function_
			dw _ref_%[_link_num_]_link - $
		%endif
		dw %4
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
			assign {0}, {this->lisp_nextsym}

			;interned symbols set and enviroments
			slot_function symbol, compare
			static_call unordered_set, create, {@_function_, 31}, {this->lisp_symbols}
			static_call unordered_map, create, {$match_obj, 31}, {this->lisp_enviroment}
			static_call unordered_map, create, {$match_obj, 31}, {this->lisp_macros}

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
					static_call lisp, built_in_func, {this, symbol, *path_ptr, table->built_in_flags}
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
		built_in "&rest", lisp_sym_rest
		built_in "&optional", lisp_sym_optional
		built_in "nil", lisp_sym_nil
		built_in "t", lisp_sym_t

		built_in "cond", 0, func_cond, 1
		built_in "def", 0, func_def, 1
		built_in "if", 0, func_if, 1
		built_in "lambda", lisp_sym_lambda, func_lambda, 1
		built_in "quote", lisp_sym_quote, func_quote, 1
		built_in "quasi-quote", lisp_sym_qquote, func_qquote, 1
		built_in "defmacro", lisp_sym_defmacro, func_defmacro, 1
		built_in "set", 0, func_set, 1
		built_in "setl", 0, func_setl, 1
		built_in "while", 0, func_while, 1

		built_in "unquote", lisp_sym_unquote
		built_in "unquote-splicing", lisp_sym_splicing
		built_in "cat", lisp_sym_cat, func_cat
		built_in "list", lisp_sym_list, func_list

		built_in "macroexpand-1", 0, func_macroexpand_1
		built_in "macroexpand", 0, func_macroexpand
		built_in "gensym", 0, func_gensym
		built_in "copy", 0, func_copy
		built_in "add", 0, func_add
		built_in "div", 0, func_div
		built_in "env", 0, func_env
		built_in "eq", 0, func_eq
		built_in "every", 0, func_every
		built_in "length", 0, func_length
		built_in "lt", 0, func_lt
		built_in "map", 0, func_map
		built_in "mod", 0, func_mod
		built_in "mul", 0, func_mul
		built_in "not", 0, func_not
		built_in "notany", 0, func_notany
		built_in "notevery", 0, func_notevery
		built_in "prin", 0, func_prin
		built_in "print", 0, func_print
		built_in "progn", 0, func_progn
		built_in "reduce", 0, func_reduce
		built_in "some", 0, func_some
		built_in "str", 0, func_str
		built_in "sub", 0, func_sub
		built_in "elem", 0, func_elem
		dw 0

	def_function_end
