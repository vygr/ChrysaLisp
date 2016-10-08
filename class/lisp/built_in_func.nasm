%include 'inc/func.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/built_in_func
		;inputs
		;r0 = lisp object
		;r1 = symbol
		;r2 = function pointer
		;r3 = function flags
		;outputs
		;r0 = lisp object

		ptr this, symbol, func_ptr, func_flags, func

		push_scope
		retire {r0, r1, r2, r3}, {this, symbol, func_ptr, func_flags}

		static_call boxed_ptr, create, {}, {func}
		static_call boxed_ptr, set_value, {func, func_ptr}
		static_call boxed_ptr, set_flags, {func, func_flags}
		static_call lisp, env_def, {this, symbol, func}
		static_call ref, deref, {func}

		eval {this}, {r0}
		pop_scope
		return

	def_function_end
