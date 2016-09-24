%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/list
		;inputs
		;r0 = globals
		;r1 = args
		;outputs
		;r0 = 0, else value

		ptr globals, args

		push_scope
		retire {r0, r1}, {globals, args}

		;eval args
		static_call lisp, repl_eval_list, {globals, args}, {args}
		if {args}
			static_call ref, ref, {args}
		endif

		eval {args}, {r0}
		pop_scope
		return

	def_function_end
