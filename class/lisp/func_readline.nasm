%include 'inc/func.ninc'
%include 'inc/load.ninc'
%include 'class/class_vector.ninc'
%include 'class/class_stream_str.ninc'
%include 'class/class_string.ninc'
%include 'class/class_error.ninc'
%include 'class/class_lisp.ninc'

def_func class/lisp/func_readline
	;inputs
	;r0 = lisp object
	;r1 = args
	;outputs
	;r0 = lisp object
	;r1 = value

	ptr this, args, value
	pubyte reloc
	long length

	push_scope
	retire {r0, r1}, {this, args}

	devirt_call vector, get_length, {args}, {length}
	if {length == 1}
		func_call vector, get_element, {args, 0}, {value}
		if {value->obj_vtable == @class/class_stream_str}
			func_path sys_load, statics
			assign {@_function_.ld_statics_reloc_buffer}, {reloc}
			func_call stream_str, read_line, {value, reloc, ld_reloc_size}, {length}
			if {length == -1}
				assign {this->lisp_sym_nil}, {value}
				func_call ref, ref, {value}
			else
				func_call string, create_from_buffer, {reloc, length}, {value}
			endif
		else
			func_call error, create, {"(read-line stream) not a stream", args}, {value}
		endif
	else
		func_call error, create, {"(read-line stream) wrong number of args", args}, {value}
	endif

	expr {this, value}, {r0, r1}
	pop_scope
	return

def_func_end
