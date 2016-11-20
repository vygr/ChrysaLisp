%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_stream_str.ninc'
%include 'class/class_string.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_write
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value, stream
	long length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 2}
		func_call vector, get_element, {args, 0}, {stream}
		vpif {stream->obj_vtable == @class/class_stream_str}
			func_call vector, get_element, {args, 1}, {value}
			vpif {value->obj_vtable == @class/class_string}
				func_call ref, ref, {value}
				func_call stream_str, write, {stream, &value->string_data, value->string_length}
			else
				func_call error, create, {"(write stream str) not a string", args}, {value}
			endif
		else
			func_call error, create, {"(write stream str) not a stream", args}, {value}
		endif
	else
		func_call error, create, {"(write stream str) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
