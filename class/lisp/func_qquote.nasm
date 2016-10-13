%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_qquote
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		def_structure pdata
			ptr pdata_this
			ptr pdata_list
		def_structure_end

		struct pdata, pdata
		ptr args, value
		ulong length

		push_scope
		retire {r0, r1}, {pdata.pdata_this, args}

		slot_call vector, get_length, {args}, {length}
		if {length == 2}
			slot_call vector, get_element, {args, 1}, {args}
			switch
			case {args->obj_vtable == @class/class_vector}
				static_call vector, create, {}, {pdata.pdata_list}
				assign {pdata.pdata_this->lisp_sym_cat}, {value}
				static_call ref, ref, {value}
				static_call vector, push_back, {pdata.pdata_list, value}
				static_call vector, for_each, {args, 0, $callback, &pdata}, {_}
				static_call lisp, repl_eval, {pdata.pdata_this, pdata.pdata_list}, {value}
				static_call ref, deref, {pdata.pdata_list}
				break
			case {args->obj_vtable == @class/class_symbol}
				static_call vector, create, {}, {value}
				assign {pdata.pdata_this->lisp_sym_quote}, {pdata.pdata_list}
				static_call ref, ref, {pdata.pdata_list}
				static_call vector, push_back, {value, pdata.pdata_list}
				static_call ref, ref, {args}
				static_call vector, push_back, {value, args}
				break
			default
				static_call ref, ref, {args}
				assign {args}, {value}
			endswitch
		else
			static_call lisp, error, {pdata.pdata_this, "(quasi-quote arg) wrong numbers of args", args}
			assign {0}, {value}
		endif

		eval {pdata.pdata_this, value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = element iterator
		;r1 = predicate data pointer
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, llist, qlist, elem
		ulong length

		push_scope
		retire {r0, r1}, {iter, pdata}

		assign {*iter}, {elem}
		if {elem->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {elem}, {length}
			gotoif {length != 2}, list_quote
			static_call vector, get_element, {elem, 0}, {qlist}
			switch
			case {qlist == pdata->pdata_this->lisp_sym_unquote}
				slot_call vector, ref_element, {elem, 1}, {qlist}
				static_call vector, create, {}, {llist}
				static_call vector, push_back, {pdata->pdata_list, llist}
				assign {pdata->pdata_this->lisp_sym_list}, {elem}
				static_call ref, ref, {elem}
				static_call vector, push_back, {llist, elem}
				static_call vector, push_back, {llist, qlist}
				break
			case {qlist == pdata->pdata_this->lisp_sym_splicing}
				slot_call vector, ref_element, {elem, 1}, {qlist}
				static_call vector, push_back, {pdata->pdata_list, qlist}
				break
			default
				goto list_quote
			endswitch
		else
		list_quote:
			static_call ref, ref, {elem}
			static_call vector, create, {}, {llist}
			static_call vector, push_back, {pdata->pdata_list, llist}
			assign {pdata->pdata_this->lisp_sym_list}, {elem}
			static_call ref, ref, {elem}
			static_call vector, push_back, {llist, elem}
			static_call vector, create, {}, {qlist}
			static_call vector, push_back, {llist, qlist}
			assign {pdata->pdata_this->lisp_sym_quote}, {elem}
			static_call ref, ref, {elem}
			static_call vector, push_back, {qlist, elem}
			static_call vector, push_back, {qlist, *iter}
		endif

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
