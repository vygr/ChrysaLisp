%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/env_push
		;inputs
		;r0 = lisp object
		;outputs
		;r0 = lisp object

		ptr this, env

		push_scope
		retire {r0}, {this}

		static_call unordered_map, create, {@class/string/compare, 1}, {env}
		static_call unordered_map, insert, {env, this->lisp_sym_parent, this->lisp_enviroment}, {_, _}
		assign {env}, {this->lisp_enviroment}

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
