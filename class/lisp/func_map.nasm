%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_map
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struc pdata
		ptr pdata_type
		ulong pdata_length
	def_struc_end

	struct pdata, pdata
	ptr this, args, value, form, func, elem
	pptr iter
	ulong length, seq_num, list_num

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length >= 2}
		func_call vector, get_element, {args, 1}, {func}
		func_path class, sequence
		func_call obj, inst_of, {func, @_function_}, {pdata.pdata_type}
		if {pdata.pdata_type}
			assign {1000000}, {pdata.pdata_length}
			func_call vector, get_element, {args, 0}, {func}
			func_call vector, for_each, {args, 1, length, $callback, &pdata}, {iter}
			ifnot {iter}
				func_call vector, create, {}, {value}
				breakifnot {pdata.pdata_length}
				func_call vector, set_capacity, {value, pdata.pdata_length}
				assign {0}, {seq_num}
				devirt_call vector, slice, {args, 1, length}, {form}
				loop_start
					assign {1}, {list_num}
					loop_start
						func_call vector, get_element, {args, list_num}, {elem}
						virt_call sequence, ref_element, {elem, seq_num}, {elem}
						func_call vector, set_element, {form, elem, list_num - 1}
						assign {list_num + 1}, {list_num}
					loop_until {list_num == length}
					func_call lisp, repl_apply, {this, func, form}, {elem}
					func_call vector, push_back, {value, elem}
					assign {seq_num + 1}, {seq_num}
				loop_until {seq_num == pdata.pdata_length}
				func_call ref, deref, {form}
			else
				func_call error, create, {"(map func list ...) not matching types", args}, {value}
			endif
		else
			func_call error, create, {"(map func list ...) not a sequence", args}, {value}
		endif
	else
		func_call error, create, {"(map func list ...) not enough args", args}, {value}
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
	ptr pdata, type
	ulong length

	push_scope
	retire {r0, r1}, {pdata, iter}

	assign {(*iter)->obj_vtable}, {type}
	if {type == pdata->pdata_type}
		virt_call sequence, get_length, {*iter}, {length}
		if {length < pdata->pdata_length}
			assign {length}, {pdata->pdata_length}
		endif
		eval {1}, r1
	else
		eval {0}, r1
	endif

	pop_scope
	return

def_func_end
