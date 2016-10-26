%include 'inc/func.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/env_def
		;inputs
		;r0 = lisp object
		;r1 = symbol
		;r2 = value
		;outputs
		;r0 = lisp object

		ptr this, symbol, value

		push_scope
		retire {r0, r1, r2}, {this, symbol, value}

		func_call unordered_map, insert, {this->lisp_enviroment, symbol, value}, {_, _}

		eval {this}, {r0}
		pop_scope
		return

	def_func_end
