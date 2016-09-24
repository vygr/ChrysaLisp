%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/sym_intern
		;inputs
		;r0 = lisp globals
		;r1 = symbol
		;outputs
		;r0 = interned symbol

		ptr lisp, symbol
		pptr iter

		push_scope
		retire {r0, r1}, {lisp, symbol}

		static_call unordered_set, insert, {lisp->lisp_symbols, symbol}, {iter, _}
		static_call string, deref, {symbol}
		assign {*iter}, {symbol}
		static_call string, ref, {symbol}

		eval {symbol}, {r0}
		pop_scope
		return

	def_function_end
