%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_boxed_long.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_push
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	int length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length > 1}
		devirt_call vector, ref_element, {args, 0}, {value}
		if {value->obj_vtable == @class/class_vector}
			func_call vector, for_each, {args, 1, length, $callback, value}, {_}
		else
			func_call error, create, {"(push list form ...) not a list", args}, {value}
		endif
	else
		func_call error, create, {"(push list form ...) not enough args", args}, {value}
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
	ptr pdata

	push_scope
	retire {r0, r1}, {pdata, iter}

	func_call ref, ref, {*iter}
	func_call vector, push_back, {pdata, *iter}

	eval {1}, r1
	pop_scope
	return

def_func_end
