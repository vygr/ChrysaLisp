%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl
		;inputs
		;r0 = lisp object
		;r1 = stream to read
		;outputs
		;r0 = lisp object

		const char_lf, 10

		ptr this, stream, ast, value
		ulong char, flag

		push_scope
		retire {r0, r1}, {this, stream}

		static_call stream, read_char, {stream}, {char}
		loop_start
			method_call stream, write_flush, {this->lisp_stderr}
			static_call sys_task, yield
			method_call stream, write_flush, {this->lisp_stdout}

			static_call lisp, repl_read, {this, stream, char}, {ast, char}
			breakif {char == -1}
			continueifnot {ast}

			if {stream == this->lisp_stdin}
				static_call stream, write_cstr, {this->lisp_stderr, "--Ast--"}
				static_call stream, write_char, {this->lisp_stderr, char_lf}
				static_call lisp, repl_print, {this, this->lisp_stderr, ast}
				static_call stream, write_char, {this->lisp_stderr, char_lf}
				static_call stream, write_cstr, {this->lisp_stderr, "--Macro expanding--"}
				static_call stream, write_char, {this->lisp_stderr, char_lf}
			endif

			loop_start
				static_call lisp, repl_expand, {this, &ast}, {flag}
				if {stream == this->lisp_stdin}
					static_call lisp, repl_print, {this, this->lisp_stderr, ast}
					static_call stream, write_char, {this->lisp_stderr, 10}
				endif
			loop_until {flag}

			if {stream == this->lisp_stdin}
				static_call stream, write_cstr, {this->lisp_stderr, "--Eval--"}
				static_call stream, write_char, {this->lisp_stderr, char_lf}
			endif

			static_call lisp, repl_eval, {this, ast}, {value}
			static_call ref, deref, {ast}
			continueifnot {value}

			if {stream == this->lisp_stdin}
				static_call lisp, repl_print, {this, this->lisp_stdout, value}
				static_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			static_call ref, deref, {value}
		loop_end

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
