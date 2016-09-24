%include 'cmd/lisp/lisp.inc'

	def_function cmd/lisp/func_def
		;inputs
		;r0 = globals
		;r1 = args
		;outputs
		;r0 = 0, else value

		ptr globals, args, vars
		ulong length

		push_scope
		retire {r0, r1}, {globals, args}

		static_call vector, get_length, {args}, {length}
		if {length != 2}
			static_call lisp, error, {globals, "(def vars vals) wrong numbers of args"}
			assign {0}, {args}
		else
			static_call vector, get_element, {args, 0}, {vars}
			static_call vector, get_element, {args, 1}, {args}
			static_call lisp, repl_eval_list, {globals, args}, {args}
			breakif {!args}
			static_call lisp, env_set_list, {globals, vars, args}, {args}
			if {args}
				static_call ref, ref, {args}
			endif
		endif

		eval {args}, {r0}
		pop_scope
		return

	def_function_end
