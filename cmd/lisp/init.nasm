%include 'inc/func.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'inc/string.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/init
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
			static_call lisp, built_in_func, {this, this->lisp_sym_def, @cmd/lisp/func_def}
			static_call lisp, built_in_func, {this, this->lisp_sym_quote, @cmd/lisp/func_quote}
			static_call lisp, built_in_func, {this, this->lisp_sym_list, @cmd/lisp/func_list}
			static_call lisp, built_in_func, {this, this->lisp_sym_lambda, @cmd/lisp/func_lambda}
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
		db 0

	def_function_end
