%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_read_qquote
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = next char
		;outputs
		;r0 = lisp object
		;r1 = list
		;r2 = next char

		ptr this, stream, list, elem
		ulong char

		push_scope
		retire {r0, r1, r2}, {this, stream, char}

		;skip "`"
		static_call stream, read_char, {stream}, {char}

		static_call vector, create, {}, {list}
		assign {this->lisp_sym_qquote}, {elem}
		static_call ref, ref, {elem}
		static_call vector, push_back, {list, elem}
		static_call lisp, repl_read, {this, stream, char}, {elem, char}
		if {elem->obj_vtable != @class/class_error}
			static_call vector, push_back, {list, elem}
		else
			static_call ref, deref, {list}
			assign {elem}, {list}
		endif

		eval {this, list, char}, {r0, r1, r2}
		pop_scope
		return

	def_function_end
