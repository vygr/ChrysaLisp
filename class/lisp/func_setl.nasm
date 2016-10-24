%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_setl
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, vars, vals
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 0}, {vars}
			static_call vector, get_element, {args, 1}, {args}
			if {args->obj_vtable == @class/class_vector}
				slot_call vector, get_length, {args}, {length}
				slot_call vector, slice, {args, 0, length}, {args}
				static_call lisp, repl_eval_list, {this, args, 0}, {vals}
				if {vals->obj_vtable != @class/class_error}
					static_call ref, deref, {vals}
					static_call lisp, env_setl_list, {this, vars, vals}, {vals}
				endif
				static_call ref, deref, {args}
			else
				static_call error, create, {"(setl vars vals) vals is not a list", args}, {vals}
			endif
		else
			static_call error, create, {"(setl vars vals) wrong numbers of args", args}, {vals}
		endif

		eval {this, vals}, {r0, r1}
		pop_scope
		return

	def_function_end
