%include 'inc/func.inc'
%include 'class/class_vector.inc'
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
			breakif {!length}
			static_call vector, get_element, {args, 0}, {arg1}
			static_call vector, get_element, {args, 1}, {arg2}
			assign {this->lisp_sym_nil}, {value}
			if {arg1 == arg2}
				assign {this->lisp_sym_t}, {value}
			elseif {arg1->obj_vtable != arg2->obj_vtable}
				break
			elseif {(arg1->obj_vtable == @class/class_boxed_ptr \
					|| arg1->obj_vtable == @class/class_boxed_long)}
				static_call boxed_ptr, get_value, {arg1}, {v1}
				static_call boxed_ptr, get_value, {arg2}, {v2}
				breakif {v1 != v2}
				assign {this->lisp_sym_t}, {value}
			endif
			static_call ref, ref, {value}
		else
			static_call lisp, error, {this, "(eq a1 a2) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
