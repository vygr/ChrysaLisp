%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/func_eq
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, value, arg1, arg2
		ulong length, v1, v2

		push_scope
		retire {r0, r1}, {this, args}

		devirt_call vector, get_length, {args}, {length}
		if {length == 2}
			func_call vector, get_element, {args, 0}, {arg1}
			func_call vector, get_element, {args, 1}, {arg2}
			gotoif {arg1 == arg2}, same
			assign {this->lisp_sym_nil}, {value}
			switch
			breakif {arg1->obj_vtable != arg2->obj_vtable}
			case {arg1->obj_vtable == @class/class_string}
				func_call string, compare, {arg1, arg2}, {length}
				gotoifnot {length}, same
				break
			case {arg1->obj_vtable == @class/class_boxed_ptr \
				|| arg1->obj_vtable == @class/class_boxed_long}
				func_call boxed_ptr, get_value, {arg1}, {v1}
				func_call boxed_ptr, get_value, {arg2}, {v2}
				breakif {v1 != v2}
			same:
				assign {this->lisp_sym_t}, {value}
			endswitch
			func_call ref, ref, {value}
		else
			func_call error, create, {"(eq form form) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_func_end
