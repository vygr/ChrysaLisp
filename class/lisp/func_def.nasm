%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_def
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, vars, vals
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {vals}
		static_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, get_element, {args, 1}, {vars}
			static_call vector, get_element, {args, 2}, {args}
			if {args->obj_vtable == @class/class_vector}
				static_call vector, get_length, {args}, {length}
				static_call vector, slice, {args, 0, length}, {args}
				static_call lisp, repl_eval_list, {this, args}, {vals}
				jmpifnot {vals}, error
				static_call lisp, env_def_list, {this, vars, vals}, {vals}
				breakif {vals}
			error:
				static_call ref, deref, {args}
			else
				static_call lisp, error, {this, "(def vars vals) vals is not a list", args}
			endif
		else
			static_call lisp, error, {this, "(def vars vals) wrong numbers of args", args}
		endif

		eval {this, vals}, {r0, r1}
		pop_scope
		return

	def_function_end
