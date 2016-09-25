%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_pair.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/env_pop
		;inputs
		;r0 = lisp object
		;outputs
		;r0 = lisp object

		ptr this, env
		pptr iter

		push_scope
		retire {r0}, {this}

		static_call unordered_map, find, {this->lisp_enviroment, this->lisp_sym_parent}, {iter, _}
		if {iter}
			static_call pair, ref_second, {*iter}, {env}
			static_call unordered_map, deref, {this->lisp_enviroment}
			assign {env}, {this->lisp_enviroment}
		endif

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
