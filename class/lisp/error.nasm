%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/error
		;inputs
		;r0 = lisp object
		;r1 = error string
		;r2 = 0, else ast
		;outputs
		;r0 = lisp object

		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_al, '<'
		const char_ar, '>'

		ptr this, error, ast, stderr

		push_scope
		retire {r0, r1, r2}, {this, error, ast}

		assign {this->lisp_stderr}, {stderr}
		static_call stream, write_cstr, {stderr, "Error: < "}
		static_call stream, write_cstr, {stderr, error}
		static_call stream, write_cstr, {stderr, " >"}
		static_call stream, write_char, {stderr, char_lf}
		if {ast}
			static_call stream, write_cstr, {stderr, "Ast: < "}
			static_call lisp, repl_print, {this, stderr, ast}
			static_call stream, write_char, {stderr, char_ar}
			static_call stream, write_char, {stderr, char_lf}
		endif
		eval {this}, {r0}
		pop_scope
		return

	def_function_end
