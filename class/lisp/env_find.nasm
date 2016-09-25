%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/env_find
		;inputs
		;r0 = lisp object
		;r1 = symbol
		;outputs
		;r0 = lisp object
		;r1 = 0, else iterator
		;r2 = bucket vector

		ptr this, symbol, bucket, env
		pptr iter

		push_scope
		retire {r0, r1}, {this, symbol}

		assign {this->lisp_enviroment}, {env}
		loop_start
			static_call unordered_map, find, {env, symbol}, {iter, bucket}
			breakif {iter}
			static_call unordered_map, find, {env, this->lisp_sym_parent}, {iter, bucket}
			breakif {!iter}
			static_call pair, get_second, {*iter}, {env}
		loop_end

		eval {this, iter, bucket}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
