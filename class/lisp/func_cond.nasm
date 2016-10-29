%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_cond
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	def_struct pdata
		ptr pdata_this
		ptr pdata_value
	def_struct_end

	struct pdata, pdata
	ptr args
	ulong length

	push_scope
	retire {r0, r1}, {pdata.pdata_this, args}

	assign {pdata.pdata_this->lisp_sym_nil}, {pdata.pdata_value}
	func_call ref, ref, {pdata.pdata_value}
	devirt_call vector, get_length, {args}, {length}
	func_call vector, for_each, {args, 0, length, $callback, &pdata}, {_}

	eval {pdata.pdata_this, pdata.pdata_value}, {r0, r1}
	pop_scope
	return

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	pptr iter
	ptr pdata, test
	ulong length

	push_scope
	retire {r0, r1}, {pdata, iter}

	if {(*iter)->obj_vtable == @class/class_vector}
		devirt_call vector, get_length, {*iter}, {length}
		if {length}
			func_call vector, get_element, {*iter, 0}, {test}
			func_call lisp, repl_eval, {pdata->pdata_this, test}, {test}
			if {test->obj_vtable == @class/class_error}
				func_call ref, deref, {pdata->pdata_value}
				assign {test}, {pdata->pdata_value}
				eval {0}, {r1}
				return
			endif
			if {test != pdata->pdata_this->lisp_sym_nil}
				func_call ref, deref, {test}
				func_call vector, for_each, {*iter, 1, length, $callback1, pdata}, {_}
				eval {0}, {r1}
				return
			else
				func_call ref, deref, {test}
				eval {1}, {r1}
				return
			endif
		else
			func_call ref, deref, {pdata->pdata_value}
			func_call error, create, {"(cond (tst form ...) ...) clause wrong number of args", *iter}, {pdata->pdata_value}
		endif
	else
		func_call ref, deref, {pdata->pdata_value}
		func_call error, create, {"(cond (tst form ...) ...) clause not list", *iter}, {pdata->pdata_value}
	endif

	eval {0}, {r1}
	pop_scope
	return

callback1:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	pptr iter
	ptr pdata

	push_scope
	retire {r0, r1}, {pdata, iter}

	func_call ref, deref, {pdata->pdata_value}
	func_call lisp, repl_eval, {pdata->pdata_this, *iter}, {pdata->pdata_value}

	eval {pdata->pdata_value->obj_vtable != @class/class_error}, {r1}
	pop_scope
	return

def_func_end
