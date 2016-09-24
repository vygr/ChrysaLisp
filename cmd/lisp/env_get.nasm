%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_get
		;inputs
		;r0 = lisp globals
		;r1 = symbol
		;outputs
		;r0 = 0, else value

		ptr lisp, symbol
		pptr iter

		push_scope
		retire {r0, r1}, {lisp, symbol}

		static_call lisp, env_find, {lisp, symbol}, {iter, _}
		assign {0}, {symbol}
		if {iter}
			static_call pair, ref_second, {*iter}, {symbol}
		endif

		eval {symbol}, {r0}
		pop_scope
		return

	def_function_end
