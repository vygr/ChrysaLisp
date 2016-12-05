%include 'inc/func.ninc'
%include 'class/class_unordered_set.ninc'
%include 'class/class_unordered_map.ninc'
%include 'class/class_symbol.ninc'
%include 'class/class_lisp.ninc'

def_struct built_in
	ushort built_in_symbol
	ushort built_in_field
	ushort built_in_function
	ushort built_in_flags
	offset built_in_hop
	;built_in_size would be aligned to long !
def_struct_end

%macro built_in 2-4 "", type_args_eval_apply
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
		func_path lisp, %3
		fn_find_link _function_
		dw _ref_%[_link_num_]_link - $
	%endif
	dw %4
%endmacro

def_func class/lisp/init
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
	vpif {ok}
		;init myself
		assign {stdin}, {this->lisp_stdin}
		assign {stdout}, {this->lisp_stdout}
		assign {stderr}, {this->lisp_stderr}
		assign {0}, {this->lisp_nextsym}

		;interned symbols set and enviroments
		func_path symbol, compare
		func_call unordered_set, create, {@_function_, 101}, {this->lisp_symbols}
		func_call unordered_map, create, {$match_obj, 31}, {this->lisp_enviroment}
		func_call unordered_map, create, {$match_obj, 31}, {this->lisp_macros}

		;intern standard built in symbols
		;fixup built in functions
		assign {$built_ins}, {table}
		loop_while {table->built_in_symbol}
			assign {&table->built_in_symbol + table->built_in_symbol}, {path_ptr}
			func_call lisp, sym_intern_cstr, {this, path_ptr}, {symbol}
			vpif {table->built_in_field}
				assign {this + table->built_in_field}, {path_ptr}
				assign {symbol}, {*path_ptr}
			endif
			vpif {table->built_in_function}
				assign {&table->built_in_function + table->built_in_function}, {path_ptr}
				func_call lisp, built_in_func, {this, symbol, *path_ptr, table->built_in_flags}
			endif
			assign {table + built_in_hop}, {table}
		loop_end

		;standard self evaulating symbols
		func_call unordered_map, insert, {this->lisp_enviroment, this->lisp_sym_nil, this->lisp_sym_nil}, {_, _}
		func_call unordered_map, insert, {this->lisp_enviroment, this->lisp_sym_t, this->lisp_sym_t}, {_, _}
	endif

	expr {this, ok}, {r0, r1}
	pop_scope
	return

match_obj:
	;inputs
	;r0 = object 1
	;r1 = object 2
	;outputs
	;r1 = 0 if match

	vpif r0, ==, r1
		vp_xor r1, r1
	endif
	vp_ret

;;;;;;;;;;;
; built ins
;;;;;;;;;;;

	align 2, db 0
built_ins:
	built_in "*parent*", lisp_sym_parent
	built_in "&rest", lisp_sym_rest
	built_in "&optional", lisp_sym_optional
	built_in "nil", lisp_sym_nil
	built_in "t", lisp_sym_t
	built_in "list", lisp_sym_list

	built_in "lambda", lisp_sym_lambda, func_lambda, type_apply

	built_in "defq", 0, func_defq, type_args_apply
	built_in "setq", 0, func_setq, type_args_apply
	built_in "cond", 0, func_cond, type_args_apply
	built_in "defmacro", 0, func_defmacro, type_args_apply
	built_in "while", 0, func_while, type_args_apply
	built_in "quote", lisp_sym_quote, func_quote, type_args_apply
	built_in "quasi-quote", lisp_sym_qquote, func_qquote, type_args_apply

	built_in "unquote", lisp_sym_unquote
	built_in "unquote-splicing", lisp_sym_splicing
	built_in "cat", lisp_sym_cat, func_cat

	built_in "def", 0, func_def
	built_in "set", 0, func_set
	built_in "macroexpand-1", 0, func_macroexpand_1
	built_in "macroexpand", 0, func_macroexpand
	built_in "gensym", 0, func_gensym
	built_in "copy", 0, func_copy
	built_in "add", 0, func_add
	built_in "div", 0, func_div
	built_in "env", 0, func_env
	built_in "eql", 0, func_eql
	built_in "length", 0, func_length
	built_in "eq", 0, func_eq
	built_in "ne", 0, func_ne
	built_in "lt", 0, func_lt
	built_in "gt", 0, func_gt
	built_in "le", 0, func_le
	built_in "ge", 0, func_ge
	built_in "bit-shr", 0, func_bshr
	built_in "bit-asr", 0, func_basr
	built_in "bit-shl", 0, func_bshl
	built_in "bit-and", 0, func_band
	built_in "bit-or", 0, func_bor
	built_in "bit-xor", 0, func_bxor
	built_in "push", 0, func_push
	built_in "pop", 0, func_pop
	built_in "mod", 0, func_mod
	built_in "mul", 0, func_mul
	built_in "not", 0, func_not
	built_in "prin", 0, func_prin
	built_in "print", 0, func_print
	built_in "progn", 0, func_progn
	built_in "apply", 0, func_apply
	built_in "str", 0, func_str
	built_in "sym", 0, func_sym
	built_in "slice", 0, func_slice
	built_in "sub", 0, func_sub
	built_in "elem", 0, func_elem
	built_in "elem-set", 0, func_elemset
	built_in "char", 0, func_char
	built_in "code", 0, func_code
	built_in "file-stream", 0, func_filestream
	built_in "string-stream", 0, func_strstream
	built_in "read-char", 0, func_readchar
	built_in "read-line", 0, func_readline
	built_in "write-char", 0, func_writechar
	built_in "write-line", 0, func_writeline
	built_in "write", 0, func_write
	built_in "repl", 0, func_repl
	built_in "eval", 0, func_eval
	built_in "save", 0, func_save
	built_in "rehash", 0, func_rehash
	built_in "inst-of", 0, func_inst_of
	built_in "split", 0, func_split
	built_in "def?", 0, func_defined
	dw 0

def_func_end
