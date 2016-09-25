%include 'inc/func.inc'
%include 'class/class_string.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/sym_intern_cstr
		;inputs
		;r0 = lisp object
		;r1 = c string pointer
		;outputs
		;r0 = lisp object
		;r1 = interned symbol

		ptr this, symbol, intern

		push_scope
		retire {r0, r1}, {this, symbol}

		static_call string, create_from_cstr, {symbol}, {symbol}
		static_call lisp, sym_intern, {this, symbol}, {intern}
		static_call string, deref, {symbol}

		eval {this, intern}, {r0, r1}
		pop_scope
		return

	def_function_end
