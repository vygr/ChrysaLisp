%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/repl_eval_list
	;inputs
	;r0 = lisp object
	;r1 = list
	;r2 = start index
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, list
	pptr iter
	ulong index, length

	push_scope
	retire {r0, r1, r2}, {this, list, index}

	devirt_call vector, get_length, {list}, {length}
	func_call vector, for_each, {list, index, length, $callback, this}, {iter}
	vpif {iter}
		assign {*iter}, {list}
	endif
	func_call ref, ref, {list}

	expr {this, list}, {r0, r1}
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

	push_scope
	retire {r0, r1}, {pdata, iter}

	func_call lisp, repl_eval, {pdata, *iter}, {pdata}
	func_call ref, deref, {*iter}
	assign {pdata}, {*iter}

	expr {pdata->obj_vtable != @class/class_error}, {r1}
	pop_scope
	return

def_func_end
