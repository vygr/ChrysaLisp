%include 'inc/func.inc'
%include 'class/class_stream.inc'
%include 'class/class_vector.inc'
%include 'class/class_symbol.inc'
%include 'class/class_boxed_long.inc'
%include 'class/class_pair.inc'
%include 'class/class_unordered_set.inc'
%include 'class/class_unordered_map.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/repl_print
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

		def_structure pdata
			ptr pdata_this
			ptr pdata_stream
			uint pdata_index
			uint pdata_length
		def_structure_end

		ptr this, stream, value, elem

		push_scope
		retire {r0, r1, r2}, {this, stream, value}

		ifnot {value}
			static_call stream, write_char, {stream, char_lf}
			static_call stream, write_cstr, {stream, "*NULL*"}
			static_call stream, write_char, {stream, char_lf}
		else
			assign {value->obj_vtable}, {elem}
			switch
			case {elem == @class/class_symbol}
				static_call stream, write, {stream, &value->string_data, value->string_length}
				break
			case {elem == @class/class_string}
				static_call stream, write_char, {stream, char_double_quote}
				static_call stream, write, {stream, &value->string_data, value->string_length}
				static_call stream, write_char, {stream, char_double_quote}
				break
			case {elem == @class/class_boxed_long}
				static_call boxed_long, get_value, {value}, {elem}
				static_call symbol, create_from_long, {elem, 10}, {value}
				static_call stream, write, {stream, &value->string_data, value->string_length}
				static_call ref, deref, {value}
				break
			case {elem == @class/class_boxed_ptr}
				static_call stream, write_cstr, {stream, "#0x"}
				static_call boxed_ptr, get_value, {value}, {elem}
				static_call symbol, create_from_long, {elem, 16}, {value}
				static_call stream, write, {stream, &value->string_data, value->string_length}
				static_call ref, deref, {value}
				break
			case {elem == @class/class_pair}
				static_call stream, write_char, {stream, char_lab}
				static_call pair, get_first, {value}, {elem}
				static_call lisp, repl_print, {this, stream, elem}
				static_call stream, write_char, {stream, char_space}
				static_call pair, get_second, {value}, {elem}
				static_call lisp, repl_print, {this, stream, elem}
				static_call stream, write_char, {stream, char_rab}
				break
			case {elem == @class/class_unordered_set}
				struct pdata, pdata
				push_scope
				static_call stream, write_char, {stream, char_lsb}
				slot_call unordered_set, get_length, {value}, {pdata.pdata_length}
				assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
				static_call unordered_set, for_each, {value, $callback, &pdata}, {_, _}
				static_call stream, write_char, {stream, char_rsb}
				pop_scope
				break
			case {elem == @class/class_unordered_map}
				struct pdata, pdata
				push_scope
				static_call stream, write_char, {stream, char_lcb}
				slot_call unordered_map, get_length, {value}, {pdata.pdata_length}
				assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
				static_call unordered_map, for_each, {value, $callback, &pdata}, {_, _}
				static_call stream, write_char, {stream, char_rcb}
				pop_scope
				break
			case {elem == @class/class_vector}
				struct pdata, pdata
				push_scope
				slot_call vector, get_length, {value}, {pdata.pdata_length}
				if {pdata.pdata_length}
					static_call vector, get_element, {value, 0}, {elem}
					switch
					case {elem == this->lisp_sym_quote}
						static_call stream, write_char, {stream, char_quote}
						break
					case {elem == this->lisp_sym_qquote}
						static_call stream, write_char, {stream, char_quasi_quote}
						break
					case {elem == this->lisp_sym_unquote}
						static_call stream, write_char, {stream, char_unquote}
						break
					case {elem == this->lisp_sym_splicing}
						static_call stream, write_char, {stream, char_splicing}
						break
					default
						goto notquote
					endswitch
					static_call vector, get_element, {value, 1}, {elem}
					static_call lisp, repl_print, {this, stream, elem}
				else
				notquote:
					static_call stream, write_char, {stream, char_lrb}
					assign {this, stream, 0}, {pdata.pdata_this, pdata.pdata_stream, pdata.pdata_index}
					static_call vector, for_each, {value, 0, $callback, &pdata}, {_}
					static_call stream, write_char, {stream, char_rrb}
				endif
				pop_scope
				break
			default
				pubyte name_offset
				push_scope
				assign {elem - 1}, {name_offset}
				assign {elem - *name_offset}, {elem}
				static_call stream, write_char, {stream, char_at}
				static_call stream, write_cstr, {stream, elem}
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

		static_call lisp, repl_print, {pdata->pdata_this, pdata->pdata_stream, *iter}
		assign {pdata->pdata_index + 1}, {pdata->pdata_index}
		if {pdata->pdata_index != pdata->pdata_length}
			static_call stream, write_char, {pdata->pdata_stream, char_space}
		endif

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
