%include 'inc/func.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/deinit
		;inputs
		;r0 = object
		;trashes
		;all

		ptr this

		push_scope
		retire {r0}, {this}

		;deinit myself
		static_call ref, deref, {this->lisp_symbols}
		static_call ref, deref, {this->lisp_enviroment}
		static_call ref, deref, {this->lisp_macros}

		;deinit parent
		super_call lisp, deinit, {this}

		pop_scope
		return

	def_function_end
