%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/error
		;inputs
		;r0 = globals
		;r1 = error string

		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_al, '<'
		const char_ar, '>'

		ptr globals, error, stderr

		push_scope
		retire {r0, r1}, {globals, error}

		assign {globals->lisp_slave->slave_stderr}, {stderr}
		static_call stream, write_cstr, {stderr, "Error: <"}
		static_call stream, write_cstr, {stderr, error}
		static_call stream, write_char, {stderr, char_ar}
		static_call stream, write_char, {stderr, char_lf}

		pop_scope
		return

	def_function_end
