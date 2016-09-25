%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/repl
		;inputs
		;r0 = lisp object
		;r1 = stream to read
		;outputs
		;r0 = lisp object

		const char_lf, 10

		ptr this, stream, ast, value
		ulong char

		push_scope
		retire {r0, r1}, {this, stream}

		static_call stream, read_char, {stream}, {char}
		loop_start
			method_call stream, write_flush, {this->lisp_stdout}
			static_call sys_task, yield
			method_call stream, write_flush, {this->lisp_stderr}

			static_call lisp, repl_read, {this, stream, char}, {ast, char}
			breakif {char == -1}
			continueif {!ast}

			if {stream == this->lisp_stdin}
				static_call stream, write_cstr, {this->lisp_stdout, "--AST--"}
				static_call stream, write_char, {this->lisp_stdout, char_lf}
				static_call lisp, repl_print, {this, this->lisp_stdout, ast}
				static_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			static_call lisp, repl_eval, {this, ast}, {value}
			static_call ref, deref, {ast}
			continueif {!value}

			if {stream == this->lisp_stdin}
				static_call stream, write_cstr, {this->lisp_stdout, "--Value--"}
				static_call stream, write_char, {this->lisp_stdout, char_lf}
				static_call lisp, repl_print, {this, this->lisp_stdout, value}
				static_call stream, write_char, {this->lisp_stdout, char_lf}
			endif

			static_call ref, deref, {value}
		loop_end

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
