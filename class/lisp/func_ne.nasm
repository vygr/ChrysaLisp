%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

;ne all different

def_func class/lisp/func_ne
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struct pdata
		ptr pdata_this
		ptr pdata_value
		pptr pdata_iter
	def_struct_end

	ptr this, value
	pptr iter

	ptr args
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length > 1}
		assign {args->vector_array}, {iter}
		assign {this->lisp_sym_t}, {value}
		func_call ref, ref, {value}
		func_call vector, for_each, {args, 0, length, $callback, &this}, {_}
	else
		func_call error, create, {"(ne num num ...) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	pptr iter
	ptr pdata
	long num, num1

	push_scope
	retire {r0, r1}, {pdata, iter}

	if {(*iter)->obj_vtable == @class/class_boxed_long}
		func_call boxed_long, get_value, {*iter}, {num}
		loop_while {iter != pdata->pdata_iter}
			assign {iter - ptr_size}, {iter}
			func_call boxed_long, get_value, {*iter}, {num1}
			continueif {num != num1}
			func_call ref, deref, {pdata->pdata_value}
			assign {pdata->pdata_this->lisp_sym_nil}, {pdata->pdata_value}
			func_call ref, ref, {pdata->pdata_value}
			expr {0}, {r1}
			return
		loop_end
		expr {1}, {r1}
		return
	else
		func_call ref, deref, {pdata->pdata_value}
		func_call error, create, {"(ne num num ...) not all numbers", *iter}, {pdata->pdata_value}
	endif

	expr {0}, {r1}
	pop_scope
	return

def_func_end
