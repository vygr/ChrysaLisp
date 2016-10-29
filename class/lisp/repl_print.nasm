%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_symbol.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_pair.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_error.inc'
%include 'class/class_lisp.inc'

def_func class/lisp/repl_print
	;inputs
	;r0 = lisp object
	;r1 = stream
	;r2 = value
	;outputs
	;r0 = lisp object

	const char_minus, "-"
	const char_quote, "'"
	const char_double_quote, '"'
	const char_quasi_quote, '`'
	const char_unquote, ','
	const char_splicing, '~'
	const char_space, " "
	const char_lrb, "("
	const char_rrb, ")"
	const char_lcb, "{"
	const char_rcb, "}"
	const char_lsb, "["
	const char_rsb, "]"
	const char_lab, "<"
	const char_rab, ">"
	const char_lf, 10
	const char_at, '@'

	def_struct pdata
		ptr pdata_this
		ptr pdata_stream
		uint pdata_index
		uint pdata_length
	def_struct_end

	ptr this, stream, value, elem

	push_scope
	retire {r0, r1, r2}, {this, stream, value}

	ifnot {value}
		func_call stream, write_char, {stream, char_lf}
		func_call stream, write_cstr, {stream, "*NULL*"}
		func_call stream, write_char, {stream, char_lf}
	else
		assign {value->obj_vtable}, {elem}
		switch
		case {elem == @class/class_symbol}
			func_call stream, write, {stream, &value->string_data, value->string_length}
			break
		case {elem == @class/class_string}
			func_call stream, write_char, {stream, char_double_quote}
			func_call stream, write, {stream, &value->string_data, value->string_length}
			func_call stream, write_char, {stream, char_double_quote}
			break
		case {elem == @class/class_boxed_long}
			func_call boxed_long, get_value, {value}, {elem}
			func_call symbol, create_from_long, {elem, 10}, {value}
			func_call stream, write, {stream, &value->string_data, value->string_length}
			func_call ref, deref, {value}
			break
		case {elem == @class/class_boxed_ptr}
			func_call stream, write_cstr, {stream, "#0x"}
			func_call boxed_ptr, get_value, {value}, {elem}
			func_call symbol, create_from_long, {elem, 16}, {value}
			func_call stream, write, {stream, &value->string_data, value->string_length}
			func_call ref, deref, {value}
			break
		case {elem == @class/class_pair}
			func_call stream, write_char, {stream, char_lab}
			func_call pair, get_first, {value}, {elem}
			func_call lisp, repl_print, {this, stream, elem}
			func_call stream, write_char, {stream, char_space}
			func_call pair, get_second, {value}, {elem}
			func_call lisp, repl_print, {this, stream, elem}
			func_call stream, write_char, {stream, char_rab}
			break
		case {elem == @class/class_unordered_set}
			struct pdata, pdata
			push_scope
			func_call stream, write_char, {stream, char_lsb}
			devirt_call unordered_set, get_length, {value}, {pdata.pdata_length}
			assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
			func_call unordered_set, for_each, {value, $callback, &pdata}, {_, _}
			func_call stream, write_char, {stream, char_rsb}
			pop_scope
			break
		case {elem == @class/class_unordered_map}
			struct pdata, pdata
			push_scope
			func_call stream, write_char, {stream, char_lcb}
			devirt_call unordered_map, get_length, {value}, {pdata.pdata_length}
			assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
			func_call unordered_map, for_each, {value, $callback, &pdata}, {_, _}
			func_call stream, write_char, {stream, char_rcb}
			pop_scope
			break
		case {elem == @class/class_error}
			func_call stream, write_cstr, {stream, "Error: < "}
			func_call error, get_description, {value}, {elem}
			func_call stream, write, {stream, &elem->string_data, elem->string_length}
			func_call stream, write_cstr, {stream, " >"}
			func_call stream, write_char, {stream, char_lf}
			func_call error, get_object, {value}, {elem}
			func_call stream, write_cstr, {stream, "Ast: < "}
			func_call lisp, repl_print, {this, stream, elem}
			func_call stream, write_cstr, {stream, " >"}
			break
		case {elem == @class/class_vector}
			struct pdata, pdata
			push_scope
			devirt_call vector, get_length, {value}, {pdata.pdata_length}
			if {pdata.pdata_length}
				func_call vector, get_element, {value, 0}, {elem}
				switch
				case {elem == this->lisp_sym_quote}
					func_call stream, write_char, {stream, char_quote}
					break
				case {elem == this->lisp_sym_qquote}
					func_call stream, write_char, {stream, char_quasi_quote}
					break
				case {elem == this->lisp_sym_unquote}
					func_call stream, write_char, {stream, char_unquote}
					break
				case {elem == this->lisp_sym_splicing}
					func_call stream, write_char, {stream, char_splicing}
					break
				default
					goto notquote
				endswitch
				func_call vector, get_element, {value, 1}, {elem}
				func_call lisp, repl_print, {this, stream, elem}
			else
			notquote:
				func_call stream, write_char, {stream, char_lrb}
				assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
				func_call vector, for_each, {value, 0, pdata.pdata_length, $callback, &pdata}, {_}
				func_call stream, write_char, {stream, char_rrb}
			endif
			pop_scope
			break
		default
			pubyte name_offset
			push_scope
			assign {elem - 1}, {name_offset}
			assign {elem - *name_offset}, {elem}
			func_call stream, write_char, {stream, char_at}
			func_call stream, write_cstr, {stream, elem}
			pop_scope
		endswitch
	endif

	eval {this}, {r0}
	pop_scope
	return

callback:
	;inputs
	;r0 = predicate data pointer
	;r1 = element iterator
	;outputs
	;r1 = 0 if break, else not

	const char_space, " "

	pptr iter
	ptr pdata

	push_scope
	retire {r0, r1}, {pdata, iter}

	func_call lisp, repl_print, {pdata->pdata_this, pdata->pdata_stream, *iter}
	assign {pdata->pdata_index + 1}, {pdata->pdata_index}
	if {pdata->pdata_index != pdata->pdata_length}
		func_call stream, write_char, {pdata->pdata_stream, char_space}
	endif

	eval {1}, {r1}
	pop_scope
	return

def_func_end
