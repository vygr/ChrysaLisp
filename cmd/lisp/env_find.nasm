%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/env_find
		;inputs
		;r0 = lisp globals
		;r1 = symbol
		;outputs
		;r0 = 0, else iterator
		;r1 = bucket vector

		ptr lisp, symbol, bucket, env
		pptr iter

		push_scope
		retire {r0, r1}, {lisp, symbol}

		assign {lisp->lisp_enviroment}, {env}
		loop_start
			static_call unordered_map, find, {env, symbol}, {iter, bucket}
			breakif {iter}
			static_call unordered_map, find, {env, lisp->lisp_sym_parent}, {iter, bucket}
			breakif {!iter}
			assign {*iter}, {env}
		loop_end

		eval {iter, bucket}, {r0, r1}
		pop_scope
		return

	def_function_end
