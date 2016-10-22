%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_lt
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

		slot_call vector, get_length, {args}, {length}
		if {length == 3}
			static_call vector, get_element, {args, 1}, {arg1}
			static_call vector, get_element, {args, 2}, {arg2}
			gotoif {arg1 == arg2}, notless
			if {arg1->obj_vtable == arg2->obj_vtable}
				assign {this->lisp_sym_t}, {value}
				switch
				case {arg1->obj_vtable == @class/class_string \
					|| arg1->obj_vtable == @class/class_symbol}
					static_call string, compare, {arg1, arg2}, {length}
					gotoif {length >= 0}, notless
					break
				case {arg1->obj_vtable == @class/class_boxed_long}
					static_call boxed_ptr, get_value, {arg1}, {v1}
					static_call boxed_ptr, get_value, {arg2}, {v2}
					breakif {v1 < v2}
				notless:
					assign {this->lisp_sym_nil}, {value}
				endswitch
				static_call ref, ref, {value}
			else
				static_call error, create, {"(lt exp exp) not same types", args}, {value}
			endif
		else
			static_call error, create, {"(lt exp exp) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
