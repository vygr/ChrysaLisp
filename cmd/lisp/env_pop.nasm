%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_pop
		;inputs
		;r0 = lisp globals

		ptr lisp, env
		pptr iter

		push_scope
		retire {r0}, {lisp}

		static_call unordered_map, find, {lisp->lisp_enviroment, lisp->lisp_sym_parent}, {iter, _}
		if {iter}
			assign {*iter}, {env}
			static_call unordered_map, ref, {env}
			static_call unordered_map, deref, {lisp->lisp_enviroment}
			assign {env}, {lisp->lisp_enviroment}
		endif

		pop_scope
		return

	def_function_end
