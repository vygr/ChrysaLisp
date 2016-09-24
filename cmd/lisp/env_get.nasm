%include 'inc/func.inc'
%include 'class/class_pair.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/env_get
		;inputs
		;r0 = lisp object
		;r1 = symbol
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, symbol
		pptr iter

		push_scope
		retire {r0, r1}, {this, symbol}

		static_call lisp, env_find, {this, symbol}, {iter, _}
		assign {0}, {symbol}
		if {iter}
			static_call pair, ref_second, {*iter}, {symbol}
		endif

		eval {this, symbol}, {r0, r1}
		pop_scope
		return

	def_function_end
