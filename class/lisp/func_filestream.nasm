%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_string.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_filestream
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

		slot_call vector, get_length, {args}, {length}
		if {length == 1}
			static_call vector, get_element, {args, 0}, {value}
			if {value->obj_vtable == @class/class_string}
				static_call string, create_from_file, {&value->string_data}, {value}
				if {value}
					static_call stream_str, create, {value}, {value}
				else
					static_call error, create, {"(file-stream filename) filename not found", args}, {value}
				endif
			else
				static_call error, create, {"(file-stream filename) filename not a string", args}, {value}
			endif
		else
			static_call error, create, {"(file-stream filename) not enough args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
