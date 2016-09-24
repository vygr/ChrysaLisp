%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_push
		;inputs
		;r0 = lisp globals

		ptr lisp, env

		push_scope
		retire {r0}, {lisp}

		static_call unordered_map, create, {@class/string/compare, 1}, {env}
		static_call unordered_map, insert, {env, lisp->lisp_sym_parent, lisp->lisp_enviroment}, {_, _}
		assign {env}, {lisp->lisp_enviroment}

		pop_scope
		return

	def_function_end
