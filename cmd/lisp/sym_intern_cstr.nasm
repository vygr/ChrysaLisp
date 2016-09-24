%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/sym_intern_cstr
		;inputs
		;r0 = lisp globals
		;r1 = c string pointer
		;outputs
		;r0 = interned symbol

		ptr lisp, symbol

		push_scope
		retire {r0, r1}, {lisp, symbol}

		static_call string, create_from_cstr, {symbol}, {symbol}
		static_call lisp, sym_intern, {lisp, symbol}, {lisp}
		static_call string, deref, {symbol}

		eval {lisp}, {r0}
		pop_scope
		return

	def_function_end
