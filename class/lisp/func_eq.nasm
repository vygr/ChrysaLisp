%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_boxed_ptr.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_eq
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value, arg1, arg2
		ulong length, v1, v2

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, slice, {args, 1, length}, {args}
			static_call lisp, repl_eval_list, {this, args}, {length}
			breakifnot {length}
			static_call vector, get_element, {args, 0}, {arg1}
			static_call vector, get_element, {args, 1}, {arg2}
			jmpif {arg1 == arg2}, same
			assign {this->lisp_sym_nil}, {value}
			switch
			breakif {arg1->obj_vtable != arg2->obj_vtable}
			case {arg1->obj_vtable == @class/class_string}
				static_call string, compare, {arg1, arg2}, {length}
				jmpifnot {length}, same
				break
			case {(arg1->obj_vtable == @class/class_boxed_ptr \
				|| arg1->obj_vtable == @class/class_boxed_long)}
				static_call boxed_ptr, get_value, {arg1}, {v1}
				static_call boxed_ptr, get_value, {arg2}, {v2}
				breakif {v1 != v2}
			same:
				assign {this->lisp_sym_t}, {value}
			endswitch
			static_call ref, ref, {value}
		else
			static_call lisp, error, {this, "(eq a1 a2) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
