%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_some
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		ptr this, args, value, func, form
		pptr iter
		ulong length, seq_length, seq_num, list_num

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		slot_call vector, get_length, {args}, {length}
		if {length >= 3}
			static_call vector, get_element, {args, 1}, {func}
			assign {1000000}, {seq_length}
			static_call vector, for_each, {args, 2, $callback, &seq_length}, {iter}
			ifnot {iter}
				assign {this->lisp_sym_nil}, {value}
				static_call ref, ref, {value}
				breakifnot {seq_length}
				assign {0}, {seq_num}
				static_call vector, slice, {args, 1, length}, {form}
				loop_start
					static_call ref, deref, {value}
					assign {2}, {list_num}
					loop_start
						static_call vector, get_element, {args, list_num}, {value}
						slot_call vector, ref_element, {value, seq_num}, {value}
						static_call vector, set_element, {form, value, list_num - 1}
						assign {list_num + 1}, {list_num}
					loop_until {list_num == length}
					static_call lisp, repl_apply, {this, func, form}, {value}
					breakifnot {value}
					breakif {value != this->lisp_sym_nil}
					assign {seq_num + 1}, {seq_num}
				loop_until {seq_num == seq_length}
				static_call ref, deref, {form}
			else
				static_call lisp, error, {this, "(some func list ...) not all lists", args}
			endif
		else
			static_call lisp, error, {this, "(some func list ...) not enough args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		pulong pdata
		ulong length

		push_scope
		retire {r0, r1}, {pdata, iter}

		if {(*iter)->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {*iter}, {length}
			if {length < *pdata}
				assign {length}, {*pdata}
			endif
			eval {1}, r1
		else
			eval {0}, r1
		endif

		pop_scope
		return

	def_function_end
