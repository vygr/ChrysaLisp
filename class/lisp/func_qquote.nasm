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
			ptr pdata_cat_list
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
				static_call vector, create, {}, {pdata.pdata_cat_list}
				assign {pdata.pdata_this->lisp_sym_cat}, {value}
				static_call ref, ref, {value}
				static_call vector, push_back, {pdata.pdata_cat_list, value}
				static_call vector, for_each, {args, 0, $callback, &pdata}, {_}
				static_call lisp, repl_eval, {pdata.pdata_this, pdata.pdata_cat_list}, {value}
				static_call ref, deref, {pdata.pdata_cat_list}
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
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, elem, list, sym, quote_list
		ulong length

		push_scope
		retire {r0, r1}, {pdata, iter}

		assign {*iter}, {elem}
		if {elem->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {elem}, {length}
			gotoifnot {length}, list_quote
			static_call vector, get_element, {elem, 0}, {sym}
			switch
			case {sym == pdata->pdata_this->lisp_sym_unquote}
				static_call vector, create, {}, {list}
				assign {pdata->pdata_this->lisp_sym_list}, {sym}
				static_call ref, ref, {sym}
				static_call vector, push_back, {list, sym}
				slot_call vector, ref_element, {elem, 1}, {quote_list}
				static_call vector, push_back, {list, quote_list}
				break
			case {sym == pdata->pdata_this->lisp_sym_splicing}
				slot_call vector, ref_element, {elem, 1}, {list}
				break
			default
				struct pdata1, pdata
				push_scope
				assign {pdata->pdata_this}, {pdata1.pdata_this}
				static_call vector, create, {}, {pdata1.pdata_cat_list}
				assign {pdata->pdata_this->lisp_sym_cat}, {sym}
				static_call ref, ref, {sym}
				static_call vector, push_back, {pdata1.pdata_cat_list, sym}
				static_call vector, for_each, {elem, 0, $callback, &pdata1}, {_}
				static_call lisp, repl_eval, {pdata->pdata_this, pdata1.pdata_cat_list}, {elem}
				static_call ref, deref, {pdata1.pdata_cat_list}
				pop_scope
				gotoif {elem},list_quote1
				eval {0}, {r1}
				return
			endswitch
		else
		list_quote:
			static_call ref, ref, {elem}
		list_quote1:
			static_call vector, create, {}, {list}
			assign {pdata->pdata_this->lisp_sym_list}, {sym}
			static_call ref, ref, {sym}
			static_call vector, push_back, {list, sym}
			static_call vector, create, {}, {quote_list}
			assign {pdata->pdata_this->lisp_sym_quote}, {sym}
			static_call ref, ref, {sym}
			static_call vector, push_back, {quote_list, sym}
			static_call vector, push_back, {quote_list, elem}
			static_call vector, push_back, {list, quote_list}
		endif
		static_call vector, push_back, {pdata->pdata_cat_list, list}

		eval {1}, {r1}
		pop_scope
		return

	def_function_end
