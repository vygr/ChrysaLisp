%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/func_quote
		;inputs
		;r0 = globals
		;r1 = args
		;outputs
		;r0 = 0, else value

		ptr globals, args
		ulong length

		push_scope
		retire {r0, r1}, {globals, args}

		static_call vector, get_length, {args}, {length}
		if {length != 1}
			static_call lisp, error, {globals, "(quote arg) wrong numbers of args"}
			assign {0}, {args}
		else
			static_call vector, ref_element, {args, 0}, {args}
		endif

		eval {args}, {r0}
		pop_scope
		return

	def_function_end
