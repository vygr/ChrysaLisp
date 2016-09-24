%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/error
		;inputs
		;r0 = lisp object
		;r1 = error string
		;outputs
		;r0 = lisp object

		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_al, '<'
		const char_ar, '>'

		ptr this, error, stderr

		push_scope
		retire {r0, r1}, {this, error}

		assign {this->lisp_stderr}, {stderr}
		static_call stream, write_cstr, {stderr, "Error: <"}
		static_call stream, write_cstr, {stderr, error}
		static_call stream, write_char, {stderr, char_ar}
		static_call stream, write_char, {stderr, char_lf}

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
