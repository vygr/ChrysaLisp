%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_def
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, vars, value
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, get_element, {args, 1}, {vars}
			static_call vector, get_element, {args, 2}, {args}
			if {args->obj_vtable == @class/class_vector}
				slot_call vector, get_length, {args}, {length}
				static_call vector, slice, {args, 0, length}, {args}
				static_call lisp, repl_eval_list, {this, args, 0}, {value}
				if {value->obj_vtable != @class/class_error}
					static_call ref, deref, {value}
					static_call lisp, env_bind, {this, vars, args, 0}, {value}
				endif
				static_call ref, deref, {args}
			else
				static_call error, create, {"(def vars vals) vals is not a list", args}, {value}
			endif
		else
			static_call error, create, {"(def vars vals) wrong numbers of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
