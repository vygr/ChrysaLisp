%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_stream_str.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_str
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

		slot_call vector, get_length, {args}, {length}
		if {length == 1}
			static_call vector, get_element, {args, 0}, {args}
			if {args->obj_vtable == @class/class_string}
				assign {args}, {value}
				static_call ref, ref, {value}
			elseif {args->obj_vtable == @class/class_stream_str}
				static_call stream_str, ref_string, {args}, {value}
			else
				static_call string, create_from_cstr, {"                "}, {value}
				static_call stream_str, create, {value}, {stream}
				static_call lisp, repl_print, {this, stream, args}
				static_call stream_str, ref_string, {stream}, {value}
				static_call ref, deref, {stream}
			endif
		else
			static_call error, create, {"(str arg) wrong numbers of args", args}, {value}
		endif

		eval {this, value}, {r0, r1}
		pop_scope
		return

	def_function_end
