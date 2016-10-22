%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_reduce
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, value, func, form, seq
		ulong length, seq_num, seq_length

		push_scope
		retire {r0, r1}, {this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 3 || length == 4}
			static_call vector, get_element, {args, 1}, {func}
			static_call vector, get_element, {args, 2}, {seq}
			slot_function class, sequence
			static_call obj, inst_of, {seq, @_function_}, {form}
			if {form}
				method_call sequence, get_length, {seq}, {seq_length}
				if {(length == 3 && seq_length > 0) || length == 4}
					if {length == 4}
						slot_call vector, ref_element, {args, 3}, {value}
						assign {0}, {seq_num}
					else
						method_call sequence, ref_element, {seq, 0}, {value}
						assign {1}, {seq_num}
					endif
					breakif {seq_num >= seq_length}
					static_call vector, slice, {args, 0, 3}, {form}
					static_call ref, ref, {func}
					static_call vector, set_element, {form, func, 0}
					loop_start
						static_call vector, set_element, {form, value, 1}
						method_call sequence, ref_element, {seq, seq_num}, {value}
						static_call vector, set_element, {form, value, 2}
						static_call lisp, repl_apply, {this, func, form}, {value}
						breakif {value->obj_vtable == @class/class_error}
						assign {seq_num + 1}, {seq_num}
					loop_until {seq_num == seq_length}
					static_call ref, deref, {form}
				else
					static_call error, create, {"(reduce func list {init}) not enough elements", args}, {value}
				endif
			else
				static_call error, create, {"(reduce func list {init}) not a sequence", seq}, {value}
			endif
		else
			static_call error, create, {"(reduce func list {init}) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
