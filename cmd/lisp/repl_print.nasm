%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'cmd/lisp/class_lisp.inc'

	def_function cmd/lisp/repl_print
		;inputs
		;r0 = lisp object
		;r1 = value
		;outputs
		;r0 = lisp object

		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'

		ptr this, value, stream

		push_scope
		retire {r0, r1}, {this, value}

		assign {this->lisp_stdout}, {stream}
		if {value->obj_vtable == @class/class_string}
			;symbol
			static_call stream, write, {stream, &value->string_data, value->string_length}
		elseif {value->obj_vtable == @class/class_boxed_ptr}
			;function pointer
			static_call stream, write_cstr, {stream, "<function>"}
		elseif {value->obj_vtable == @class/class_vector}
			;list
			static_call stream, write_char, {stream, char_lb}
			static_call stream, write_char, {stream, char_space}
			static_call vector, for_each, {value, $repl_print_callback, this}, {_}
			static_call stream, write_char, {stream, char_rb}
		endif
		static_call stream, write_char, {stream, char_space}

		eval {this}, {r0}
		pop_scope
		return

	repl_print_callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr this

		push_scope
		retire {r0, r1}, {iter, this}

		static_call lisp, repl_print, {this, *iter}

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
