%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/func_def
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, vars
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		static_call vector, get_length, {args}, {length}
		if {length != 2}
			static_call lisp, error, {this, "(def vars vals) wrong numbers of args"}
			assign {0}, {args}
		else
			static_call vector, get_element, {args, 0}, {vars}
			static_call vector, get_element, {args, 1}, {args}
			static_call lisp, repl_eval_list, {this, args}, {args}
			breakif {!args}
			static_call lisp, env_set_list, {this, vars, args}, {args}
			if {args}
				static_call ref, ref, {args}
			endif
		endif

		eval {this, args}, {r0, r1}
		pop_scope
		return

	def_function_end
