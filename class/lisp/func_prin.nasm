%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_stream.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_prin
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	func_call vector, for_each, {args, 0, length, $callback, this}, {_}
	assign {this->lisp_sym_nil}, {args}
	func_call ref, ref, {args}

	expr {this, args}, {r0, r1}
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

	push_scope
	retire {r0, r1}, {pdata, iter}

	assign {*iter}, {elem}
	vpif {elem->obj_vtable == @class/class_string}
		func_call stream, write, {pdata->lisp_stdout, &elem->string_data, elem->string_length}
	else
		func_call lisp, repl_print, {pdata, pdata->lisp_stdout, elem}
	endif

	expr {1}, {r1}
	pop_scope
	return

def_func_end
