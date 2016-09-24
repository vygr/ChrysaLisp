%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/built_in_func
		;inputs
		;r0 = lisp globals
		;r1 = symbol
		;r2 = function pointer

		ptr lisp, symbol, func_ptr, func

		push_scope
		retire {r0, r1, r2}, {lisp, symbol, func_ptr}

		static_call boxed_ptr, create, {}, {func}
		static_call boxed_ptr, set_value, {func, func_ptr}
		static_call lisp, env_set, {lisp, symbol, func}
		static_call boxed_ptr, deref, {func}

		pop_scope
		return

	def_function_end
