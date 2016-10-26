%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/func_every
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = value

		ptr this, args, value, func, form
		pptr iter
		ulong length, seq_length, seq_num, list_num

		push_scope
		retire {r0, r1}, {this, args}

		devirt_call vector, get_length, {args}, {length}
		if {length >= 2}
			func_call vector, get_element, {args, 0}, {func}
			assign {1000000}, {seq_length}
			func_call vector, for_each, {args, 1, length, $callback, &seq_length}, {iter}
			ifnot {iter}
				assign {this->lisp_sym_t}, {value}
				func_call ref, ref, {value}
				breakifnot {seq_length}
				assign {0}, {seq_num}
				devirt_call vector, slice, {args, 1, length}, {form}
				loop_start
					func_call ref, deref, {value}
					assign {1}, {list_num}
					loop_start
						func_call vector, get_element, {args, list_num}, {value}
						devirt_call vector, ref_element, {value, seq_num}, {value}
						func_call vector, set_element, {form, value, list_num - 1}
						assign {list_num + 1}, {list_num}
					loop_until {list_num == length}
					func_call lisp, repl_apply, {this, func, form}, {value}
					breakif {value->obj_vtable == @class/class_error}
					breakif {value == this->lisp_sym_nil}
					assign {seq_num + 1}, {seq_num}
				loop_until {seq_num == seq_length}
				func_call ref, deref, {form}
			else
				func_call error, create, {"(every func list ...) not all lists", args}, {value}
			endif
		else
			func_call error, create, {"(every func list ...) not enough args", args}, {value}
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
			devirt_call vector, get_length, {*iter}, {length}
			if {length < *pdata}
				assign {length}, {*pdata}
			endif
			eval {1}, r1
		else
			eval {0}, r1
		endif

		pop_scope
		return

	def_func_end
