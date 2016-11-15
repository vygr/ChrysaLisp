%include 'inc/func.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_string.ninc'
%include 'class/class_stream_str.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_str
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value, stream
	ulong length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	vpif {length == 1}
		func_call vector, get_element, {args, 0}, {args}
		vpif {args->obj_vtable == @class/class_string}
			assign {args}, {value}
			func_call ref, ref, {value}
		elseif {args->obj_vtable == @class/class_stream_str}
			func_call stream_str, ref_string, {args}, {value}
		else
			func_call string, create_from_cstr, {"                "}, {value}
			func_call stream_str, create, {value}, {stream}
			func_call lisp, repl_print, {this, stream, args}
			func_call stream_str, ref_string, {stream}, {value}
			func_call ref, deref, {stream}
		endif
	else
		func_call error, create, {"(str arg) wrong numbers of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
