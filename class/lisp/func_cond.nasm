%include 'inc/func.inc'
%include 'class/class_vector.inc'
%include 'class/class_lisp.inc'

	def_function class/lisp/func_cond
		;inputs
		;r0 = lisp object
		;r1 = args
		;outputs
		;r0 = lisp object
		;r1 = 0, else value

		def_structure pdata
			ptr pdata_this
			ptr pdata_value
		def_structure_end

		struct pdata, pdata
		ptr args

		push_scope
		retire {r0, r1}, {pdata.pdata_this, args}

		assign {0}, {pdata.pdata_value}
		static_call vector, for_each, {args, 1, $callback, &pdata}, {args}
		ifnot {args}
			assign {pdata.pdata_this->lisp_sym_nil}, {pdata.pdata_value}
			static_call ref, ref, {pdata.pdata_value}
		endif

		eval {pdata.pdata_this, pdata.pdata_value}, {r0, r1}
		pop_scope
		return

	callback:
		;inputs
		;r0 = predicate data pointer
		;r1 = element iterator
		;outputs
		;r1 = 0 if break, else not

		pptr iter
		ptr pdata, test

		push_scope
		retire {r0, r1}, {pdata, iter}

		if {(*iter)->obj_vtable == @class/class_vector}
			slot_call vector, get_length, {*iter}, {test}
			if {test}
				static_call vector, get_element, {*iter, 0}, {test}
				static_call lisp, repl_eval, {pdata->pdata_this, test}, {test}
				gotoifnot {test}, error
				if {test != pdata->pdata_this->lisp_sym_nil}
					static_call ref, deref, {test}
					static_call lisp, func_progn, {pdata->pdata_this, *iter}, {pdata->pdata_value}
					eval {0}, {r1}
				else
					static_call ref, deref, {test}
					eval {1}, {r1}
				endif
			else
				static_call lisp, error, {pdata->pdata_this, "(cond (tst form ...) ...) clause wrong number of args", *iter}
				eval {0}, {r1}
			endif
		else
			static_call lisp, error, {pdata->pdata_this, "(cond (tst form ...) ...) clause not list", *iter}
		error:
			eval {0}, {r1}
		endif

		pop_scope
		return

	def_function_end
