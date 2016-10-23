%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_string.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_write
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

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			static_call vector, get_element, {args, 0}, {stream}
			if {stream->obj_vtable == @class/class_stream_str}
				static_call vector, get_element, {args, 1}, {value}
				if {value->obj_vtable == @class/class_string}
					static_call ref, ref, {value}
					static_call stream_str, write, {stream, &value->string_data, value->string_length}
				else
					static_call error, create, {"(write-line stream str) not a string", args}, {value}
				endif
			else
				static_call error, create, {"(write-line stream str) not a stream", args}, {value}
			endif
		else
			static_call error, create, {"(write-line stream str) not enough args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
