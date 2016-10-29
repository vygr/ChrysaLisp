%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/func_cat
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

	devirt_call vector, get_length, {args}, {length}
	if {length}
		func_call vector, get_element, {args, 0}, {pdata.pdata_value}
		if {pdata.pdata_value->obj_vtable == @class/class_vector}
			func_call vector, create, {}, {pdata.pdata_value}
			func_call vector, for_each, {args, 0, length, $callback, &pdata}, {_}
		elseif {pdata.pdata_value->obj_vtable == @class/class_string}
			func_call ref, ref, {pdata.pdata_value}
			func_call vector, for_each, {args, 1, length, $callback, &pdata}, {_}
		else
			func_call error, create, {"(cat seq ...) not sequence type", pdata.pdata_value}, {pdata.pdata_value}
		endif
	else
		func_call error, create, {"(cat seq ...) wrong number of args", args}, {pdata.pdata_value}
	endif

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
	ptr pdata, elem
	ulong length

	push_scope
	retire {r0, r1}, {pdata, iter}

	assign {*iter}, {elem}
	if {elem->obj_vtable == pdata->pdata_value->obj_vtable}
		switch
		case {elem->obj_vtable == @class/class_string}
			func_call string, append, {pdata->pdata_value, elem}, {elem}
			func_call ref, deref, {pdata->pdata_value}
			assign {elem}, {pdata->pdata_value}
			break
		default
			devirt_call vector, get_length, {elem}, {length}
			func_call vector, append, {pdata->pdata_value, elem, 0, length}
		endswitch
		eval {1}, {r1}
	else
		func_call ref, deref, {pdata->pdata_value}
		func_call error, create, {"(cat seq ...) not matching type", elem}, {pdata->pdata_value}
		eval {0}, {r1}
	endif

	pop_scope
	return

def_func_end
