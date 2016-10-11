%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_map
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		def_structure pdata
			ptr pdata_type
			ulong pdata_length
		def_structure_end

		struct pdata, pdata
		ptr this, args, value, form, func, elem
		pptr iter
		ulong length, seq_num, list_num

		push_scope
		retire {r0, r1}, {this, args}

		assign {0}, {value}
		slot_call vector, get_length, {args}, {length}
		if {length >= 3}
			static_call vector, get_element, {args, 2}, {func}
			slot_function class, sequence
			static_call obj, inst_of, {func, @_function_}, {pdata.pdata_type}
			if {pdata.pdata_type}
				assign {1000000}, {pdata.pdata_length}
				static_call vector, get_element, {args, 1}, {func}
				static_call vector, for_each, {args, 2, $callback, &pdata}, {iter}
				ifnot {iter}
					static_call vector, create, {}, {value}
					breakifnot {pdata.pdata_length}
					static_call vector, set_capacity, {value, pdata.pdata_length}
					assign {0}, {seq_num}
					static_call vector, slice, {args, 1, length}, {form}
					static_call ref, ref, {func}
					static_call vector, set_element, {form, func, 0}
					loop_start
						assign {2}, {list_num}
						loop_start
							static_call vector, get_element, {args, list_num}, {elem}
							method_call sequence, ref_element, {elem, seq_num}, {elem}
							static_call vector, set_element, {form, elem, list_num - 1}
							assign {list_num + 1}, {list_num}
						loop_until {list_num == length}
						static_call lisp, repl_apply, {this, func, form}, {elem}
						breakifnot {elem}
						static_call vector, push_back, {value, elem}
						assign {seq_num + 1}, {seq_num}
					loop_until {seq_num == pdata.pdata_length}
					if {seq_num != pdata.pdata_length}
						static_call ref, deref, {value}
						assign {0}, {value}
					endif
					static_call ref, deref, {form}
				else
					static_call lisp, error, {this, "(map func list ...) not matching types", args}
				endif
			else
				static_call lisp, error, {this, "(map func list ...) not a sequence", args}
			endif
		else
			static_call lisp, error, {this, "(map func list ...) not enough args", args}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, type
		ulong length

		push_scope
		retire {r0, r1}, {iter, pdata}

		assign {(*iter)->obj_vtable}, {type}
		if {type == pdata->pdata_type}
			method_call sequence, get_length, {*iter}, {length}
			if {length < pdata->pdata_length}
				assign {length}, {pdata->pdata_length}
			endif
			eval {1}, r1
		else
			eval {0}, r1
		endif

		pop_scope
		return

	def_function_end
