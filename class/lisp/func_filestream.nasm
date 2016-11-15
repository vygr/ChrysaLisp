%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_stream_str.ninc'
%include 'class/class_string.ninc'
%include 'class/class_symbol.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_filestream
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 1}
		func_call vector, get_element, {args, 0}, {value}
		if {value->obj_vtable == @class/class_string \
			|| value->obj_vtable == @class/class_symbol}
			func_call string, create_from_file, {&value->string_data}, {value}
			if {value}
				func_call stream_str, create, {value}, {value}
			else
				func_call error, create, {"(file-stream filename) filename not found", args}, {value}
			endif
		else
			func_call error, create, {"(file-stream filename) filename not a filename", args}, {value}
		endif
	else
		func_call error, create, {"(file-stream filename) not enough args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
