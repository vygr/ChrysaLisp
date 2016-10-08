%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_reduce
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value, func, form, list
		ulong length, seq_num, list_length

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		static_call vector, get_length, {args}, {length}
		if {length == 3 || length == 4}
			static_call vector, get_element, {args, 1}, {func}
			static_call vector, get_element, {args, 2}, {list}
			if {list->obj_vtable == @class/class_vector}
				static_call vector, get_length, {list}, {list_length}
				if {(length == 3 && list_length > 0) || length == 4}
					if {length == 4}
						static_call vector, get_element, {args, 3}, {value}
						static_call lisp, repl_eval, {this, value}, {value}
						assign {0}, {seq_num}
					else
						static_call vector, ref_element, {list, 0}, {value}
						assign {1}, {seq_num}
					endif
					breakifnot {value}
					breakif {seq_num >= list_length}
					static_call vector, slice, {args, 0, 3}, {form}
					static_call ref, ref, {func}
					static_call vector, set_element, {form, func, 0}
					loop_start
						static_call vector, set_element, {form, value, 1}
						static_call vector, ref_element, {list, seq_num}, {value}
						static_call vector, set_element, {form, value, 2}
						static_call lisp, repl_apply, {this, func, form}, {value}
						breakifnot {value}
						assign {seq_num + 1}, {seq_num}
					loop_until {seq_num == list_length}
					static_call ref, deref, {form}
				else
					static_call lisp, error, {this, "(reduce func list {init}) not enough elements", args}
				endif
			else
				static_call lisp, error, {this, "(reduce func list {init}) not a list", list}
			endif
		else
			static_call lisp, error, {this, "(reduce func list {init}) wrong number of args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
