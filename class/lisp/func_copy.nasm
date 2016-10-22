%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_copy
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args
		ulong length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 1}, {args}
			if {args->obj_vtable == @class/class_vector}
				slot_call vector, get_length, {args}, {length}
				static_call vector, slice, {args, 0, length}, {args}
				static_call vector, for_each, {args, 0, $callback, 0}, {_}
			else
				static_call ref, ref, {args}
			endif
		else
			static_call error, create, {"(copy form) wrong number of args", args}, {args}
		endif

		eval {this, args}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata
		ulong length

		push_scope
		retire {r0, r1}, {pdata, iter}

		assign {*iter}, {pdata}
		if {pdata->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {pdata}, {length}
			static_call vector, slice, {pdata, 0, length}, {pdata}
			static_call vector, for_each, {pdata, 0, $callback, 0}, {_}
			static_call ref, deref, {*iter}
			assign {pdata}, {*iter}
		endif

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
