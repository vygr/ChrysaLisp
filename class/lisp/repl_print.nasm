%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_string.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_boxed_ptr.inc'
%include 'inc/string.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_print
		;inputs
		;r0 = lisp object
		;r1 = stream
		;r2 = value
		;outputs
		;r0 = lisp object

		const char_space, ' '
		const char_lf, 10
		const char_lb, '('
		const char_rb, ')'
		const char_ar, '>'
		const char_minus, '-'
		const char_quote, "'"

		def_structure pdata
			ptr pdata_this
			ptr pdata_stream
		def_structure_end

		ptr this, stream, value, elem
		pubyte buffer
		long num

		push_scope
		retire {r0, r1, r2}, {this, stream, value}

		if {value->obj_vtable == @class/class_string}
			;symbol
			static_call stream, write, {stream, &value->string_data, value->string_length}
		elseif {value->obj_vtable == @class/class_boxed_long}
			;number
			static_call boxed_long, get_value, {value}, {num}
			assign {$buffer}, {buffer}
			if {num < 0}
				assign {char_minus}, {*buffer}
				assign {buffer + 1}, {buffer}
				assign {-num}, {num}
			endif
			static_call sys_string, from_long, {num, buffer, 10}
			static_call stream, write_cstr, {stream, $buffer}
		elseif {value->obj_vtable == @class/class_boxed_ptr}
			;function pointer
			static_call stream, write_cstr, {stream, "<function 0x"}
			static_call boxed_ptr, get_value, {value}, {num}
			static_call sys_string, from_long, {num, $buffer, 16}
			static_call stream, write_cstr, {stream, $buffer}
			static_call stream, write_char, {stream, char_ar}
		elseif {value->obj_vtable == @class/class_vector}
			;list
			static_call vector, get_length, {value}, {num}
			if {num}
				static_call vector, get_element, {value, 0}, {elem}
				jmpif {elem != this->lisp_sym_quote}, notquote
				static_call stream, write_char, {stream, char_quote}
				static_call vector, get_element, {value, 1}, {elem}
				static_call lisp, repl_print, {this, stream, elem}
			else
			notquote:
				struct pdata, pdata
				push_scope
				static_call stream, write_char, {stream, char_lb}
				static_call stream, write_char, {stream, char_space}
				assign {this, stream}, {pdata.pdata_this, pdata.pdata_stream}
				static_call vector, for_each, {value, 0, $repl_print_callback, &pdata}, {_}
				static_call stream, write_char, {stream, char_rb}
				pop_scope
			endif
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
		ptr pdata

		push_scope
		retire {r0, r1}, {iter, pdata}

		static_call lisp, repl_print, {pdata->pdata_this, pdata->pdata_stream, *iter}

		eval {1}, {r1}
		pop_scope
		return

	buffer:
		;static buffer for number output
		;bad idea, but we are co-op sheduled so don't yield during this !!
		times 21 db 0

	def_function_end
