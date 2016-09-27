%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'inc/string.inc'
%include 'class/class_lisp.inc'

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

		ptr this, vtable, stdin, stdout, stderr
		pubyte next_byte
		pptr built_in
		ulong ok, length

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
			static_call unordered_set, create, {@class/string/compare, 31}, {this->lisp_symbols}
			static_call unordered_map, create, {@class/string/compare, 31}, {this->lisp_enviroment}

			;intern standard built in symbols
			assign {$built_in_symbols, &this->lisp_sym_parent}, {next_byte, built_in}
			loop_while {*next_byte}
				static_call lisp, sym_intern_cstr, {this, next_byte}, {*built_in}
				static_call sys_string, length, {next_byte}, {length}
				assign {next_byte + length + 1}, {next_byte}
				assign {built_in + ptr_size}, {built_in}
			loop_end

			;standard self evaulating symbols
			static_call lisp, env_set, {this, this->lisp_sym_nil, this->lisp_sym_nil}
			static_call lisp, env_set, {this, this->lisp_sym_t, this->lisp_sym_t}

			;bind built in functions
			slot_function lisp, func_def
			static_call lisp, built_in_func, {this, this->lisp_sym_def, @_function_}
			slot_function lisp, func_quote
			static_call lisp, built_in_func, {this, this->lisp_sym_quote, @_function_}
			slot_function lisp, func_list
			static_call lisp, built_in_func, {this, this->lisp_sym_list, @_function_}
			slot_function lisp, func_lambda
			static_call lisp, built_in_func, {this, this->lisp_sym_lambda, @_function_}
			slot_function lisp, func_add
			static_call lisp, built_in_func, {this, this->lisp_sym_add, @_function_}
			slot_function lisp, func_sub
			static_call lisp, built_in_func, {this, this->lisp_sym_sub, @_function_}
			slot_function lisp, func_mul
			static_call lisp, built_in_func, {this, this->lisp_sym_mul, @_function_}
			slot_function lisp, func_div
			static_call lisp, built_in_func, {this, this->lisp_sym_div, @_function_}
			slot_function lisp, func_mod
			static_call lisp, built_in_func, {this, this->lisp_sym_mod, @_function_}
			slot_function lisp, func_eq
			static_call lisp, built_in_func, {this, this->lisp_sym_eq, @_function_}
			slot_function lisp, func_cond
			static_call lisp, built_in_func, {this, this->lisp_sym_cond, @_function_}
			slot_function lisp, func_progn
			static_call lisp, built_in_func, {this, this->lisp_sym_progn, @_function_}
			slot_function lisp, func_not
			static_call lisp, built_in_func, {this, this->lisp_sym_not, @_function_}
			slot_function lisp, func_and
			static_call lisp, built_in_func, {this, this->lisp_sym_and, @_function_}
			slot_function lisp, func_or
			static_call lisp, built_in_func, {this, this->lisp_sym_or, @_function_}
		endif

		eval {this, ok}, {r0, r1}
		pop_scope
		return

;;;;;;;;;;;;;;;;;;
; built in symbols
;;;;;;;;;;;;;;;;;;

	built_in_symbols:
		db "_parent_", 0
		db "nil", 0
		db "t", 0
		db "lambda", 0
		db "def", 0
		db "quote", 0
		db "list", 0
		db "add", 0
		db "sub", 0
		db "mul", 0
		db "div", 0
		db "mod", 0
		db "eq", 0
		db "cond", 0
		db "progn", 0
		db "not", 0
		db "and", 0
		db "or", 0
		db 0

	def_function_end
