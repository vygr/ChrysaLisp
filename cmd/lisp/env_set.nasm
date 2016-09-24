%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_set
		;inputs
		;r0 = lisp globals
		;r1 = symbol
		;r2 = value

		ptr lisp, symbol, value
		pptr iter

		push_scope
		retire {r0, r1, r2}, {lisp, symbol, value}

		static_call lisp, env_find, {lisp, symbol}, {iter, _}
		if {iter}
			;change existing value
			static_call ref, ref, {value}
			static_call pair, set_second, {*iter, value}
		else
			;new variable
			static_call unordered_map, insert, {lisp->lisp_enviroment, symbol, value}, {_, _}
		endif

		pop_scope
		return

	def_function_end
