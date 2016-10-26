%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_string.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_func class/lisp/func_writeline
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
		if {length == 2}
			func_call vector, get_element, {args, 0}, {stream}
			if {stream->obj_vtable == @class/class_stream_str}
				func_call vector, get_element, {args, 1}, {value}
				if {value->obj_vtable == @class/class_string}
					func_call ref, ref, {value}
					func_call stream_str, write, {stream, &value->string_data, value->string_length}
					func_call stream_str, write_char, {stream, 10}
				else
					func_call error, create, {"(write-line stream str) not a string", args}, {value}
				endif
			else
				func_call error, create, {"(write-line stream str) not a stream", args}, {value}
			endif
		else
			func_call error, create, {"(write-line stream str) wrong number of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_func_end
